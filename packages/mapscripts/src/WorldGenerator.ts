import { TiledMap } from "./TiledMap";
import { perlin2d } from "noise/perlin";
import {
    addChunk,
    calcWangVal,
    clamp,
    collectSubArr,
    DataGrid,
    distance,
    getWangXY,
    Grid,
} from "./utils";
import TiledRawJSON, { ITileLayer } from "joegamelib/src/types/TiledRawJson";

import { jprng2 } from "noise/ripemd160";
import Color from "color";

type SignalConfig = {
    type:
        | "perlin"
        | "fill"
        | "circle"
        | "binary"
        | "voronoi"
        | "voronoi-man"
        | "voronoi-squared";
    params: [string, number][];
    filters?: SignalConfig[];
};
type CliffLayer = {
    base_texture: string;
    layers: { texture: string; signal: SignalConfig }[];
};
type WorldConfig = {
    cliff_signal: SignalConfig;
    cliffs: CliffLayer[];
    colors: Record<string, string>;
};

export class WorldGenerator {
    signals: Signal[] = [];
    wangLayers: WangLayer[] = [];
    wangMap: TiledMap;
    cliffSystem: CliffSystem;
    constructor(tiledData: TiledRawJSON) {
        this.wangMap = new TiledMap(tiledData);
        this.cliffSystem = new CliffSystem(
            "cliffs",
            new Perlin(0.07, 5, 192132),
            this.wangMap,
            []
        );
    }

    /*
     * Takes width and coordinates in non-wang signal space.
     */
    getMap(x: number, y: number, w: number, h: number) {
        const newMap = TiledMap.createEmpty(
            h * 4,
            w * 4,
            this.wangMap.getConf()
        );
        const cliffLgs = this.cliffSystem.getAllTileLayerGrids(x, y, w, h);
        newMap.applyLgs(cliffLgs, "cliffs");
        return newMap.getConf();
    }
}

enum SignalFilterTypes {
    circle,
    snap,
    binary,
    signalmask,
    edge,
}

interface SignalFilter {
    process: (x: number, y: number, val: number) => number;
    ttype: SignalFilterTypes;
    clone: () => SignalFilter;
}

export class CircleFilter implements SignalFilter {
    ttype = SignalFilterTypes.circle;
    constructor(
        private cx: number,
        private cy: number,
        private r: number,
        private amount: number
    ) {}
    process(x: number, y: number, val: number) {
        const dist = distance(x, y, this.cx, this.cy);
        const fact = dist < this.r ? 1 + this.amount * (1 - dist / this.r) : 1;
        return clamp(fact * val, 0.07, 0.94);
    }
    clone() {
        return new CircleFilter(this.cx, this.cy, this.r, this.amount);
    }
}

export class SnapFilter implements SignalFilter {
    ttype = SignalFilterTypes.snap;
    constructor(private snaps: number) {}
    process(_x: number, _y: number, val: number) {
        let out = 0;
        const segs = Array(this.snaps)
            .fill(0)
            .map((_, idx) => idx / this.snaps);
        segs.forEach((seg) => {
            if (seg < val) out = seg;
        });
        return out;
    }

    clone() {
        return new SnapFilter(this.snaps);
    }
}

export class BinaryFilter implements SignalFilter {
    ttype = SignalFilterTypes.binary;
    constructor(private n: number) {}
    process(_x: number, _y: number, val: number) {
        if (val >= this.n) return 1;
        else return 0;
    }
    setN(n: number) {
        this.n = n;
    }
    clone() {
        return new BinaryFilter(this.n);
    }
}

export class SignalMaskFilter implements SignalFilter {
    ttype = SignalFilterTypes.signalmask;

    private sig: Signal;
    constructor(private n: number, sig: Signal) {
        this.sig = sig.clone();
        // this.sig.filters.push(new BinaryFilter(n));
    }
    process(x: number, y: number, val: number) {
        if (this.sig.get(x, y) === 1) return val;
        else return 0;
    }
    clone() {
        return new SignalMaskFilter(this.n, this.sig.clone());
    }
}

export class EdgeFilter implements SignalFilter {
    ttype = SignalFilterTypes.edge;
    private sig: Signal;
    constructor(sig: Signal) {
        this.sig = sig.clone();
    }
    process(x: number, y: number, val: number) {
        if (val === 0) return 0;
        //it is passed through when one of the neighbors is 0
        const neigbors =
            this.sig.get(x - 1, y - 1) &&
            this.sig.get(x - 1, y) &&
            this.sig.get(x - 1, y + 1) &&
            this.sig.get(x, y - 1) &&
            this.sig.get(x, y + 1) &&
            this.sig.get(x + 1, y - 1) &&
            this.sig.get(x + 1, y) &&
            this.sig.get(x + 1, y + 1);
        if (neigbors) return 0;
        else return 1;
    }
    clone() {
        return new EdgeFilter(this.sig.clone());
    }
}

export type Signal = GenericSignal;

export class GenericSignal {
    filters: SignalFilter[];
    cache: number[][];

    constructor(f?: SignalFilter[]) {
        this.filters = f || [];
        this.cache = [];
    }

    get(x: number, y: number): number {
        if (this.cache[y] && this.cache[y][x]) {
            return this.cache[y][x];
        } else {
            if (!this.cache[y]) this.cache[y] = [];
            const out = this.applySignalFilters(x, y, this.getBaseValue(x, y));
            this.cache[y][x] = out;
            // if (!out) throw Error(`Issue with signal ${x} ${y}`);
            return out;
        }
    }

    clearCache() {
        this.cache = [];
    }

    render(
        w: number,
        h: number,
        cb = (_x: number, _y: number, v: number) => v,
        xo = 0,
        yo = 0
    ): number[][] {
        return Array(h)
            .fill(0)
            .map((_row, y) =>
                Array(w)
                    .fill(0)
                    .map((_item, x) => cb(x, y, this.get(xo + x, yo + y)))
            );
    }

    async asyncRender(
        w: number,
        h: number,
        cb = (_x: number, _y: number, v: number) => v,
        xo = 0,
        yo = 0
    ) {
        return new Promise<number[][]>((res) =>
            res(this.render(w, h, cb, xo, yo))
        );
    }

    async renderToContext(
        w: number,
        h: number,
        ctx: {
            fillStyle: string | CanvasGradient | CanvasPattern;
            fillRect: (x: number, y: number, w: number, h: number) => void;
        },
        color?: string,
        xo = 0,
        yo = 0
    ) {
        return this.asyncRender(
            w,
            h,
            (x, y, val) => {
                let _color = color;
                if (!_color) {
                    const hex = (
                        "0" + Math.floor(val * 255).toString(16)
                    ).slice(-2);
                    _color = "#" + hex + hex + hex;
                } else {
                    _color = val === 1 ? color + "ff" : "#00000000";
                }
                ctx.fillStyle = _color;
                ctx.fillRect(x, y, 1, 1);
                return val;
            },
            xo,
            yo
        );
    }

    renderRect(x: number, y: number, w: number, h: number): number[][] {
        return Array(h)
            .fill(0)
            .map((_row, iy) =>
                Array(w)
                    .fill(0)
                    .map((_item, ix) => this.get(x + ix, y + iy))
            );
    }

    private applySignalFilters(x: number, y: number, val: number): number {
        const out = this.filters.reduce(
            (acc, curr) => (curr ? curr.process(x, y, acc) : acc),
            val
        );
        return out;
    }
    getBaseValue(_x: number, _y: number) {
        return 0;
    }

    clone() {
        return new GenericSignal(this.filters.map((item) => item.clone()));
    }
    static isSignal(d: GenericSignal | SignalFilter): d is GenericSignal {
        return (d as GenericSignal).render !== undefined;
    }
}

export class FillSignal extends GenericSignal {
    n: number;
    constructor(n?: number) {
        super([]);
        this.n = n || 1;
    }
    getBaseValue(_x: number, _y: number) {
        return this.n;
    }
    clone() {
        return new FillSignal(this.n);
    }
}

export class Perlin extends GenericSignal {
    freq: number;
    depth: number;
    seed: number;
    constructor(
        freq: number,
        depth: number,
        seed = 0,
        filters?: SignalFilter[]
    ) {
        super(filters);
        this.freq = freq;
        this.depth = depth;
        this.seed = seed;
    }
    getBaseValue(x: number, y: number) {
        return perlin2d(x, y, this.freq, this.depth, this.seed);
    }

    clone() {
        return new Perlin(
            this.freq,
            this.depth,
            this.seed,
            this.filters.map((f) => f.clone())
        );
    }
}

export class Voronoi extends GenericSignal {
    size: number;
    constructor(size: number, filters?: SignalFilter[]) {
        super(filters);
        this.size = size;
    }

    getBaseValue(x: number, y: number): number {
        const cx = Math.floor(x / this.size);
        const cy = Math.floor(y / this.size);

        let minDist = Infinity,
            minDistSecond = Infinity;

        for (let i = -1; i <= 1; i++) {
            for (let j = -1; j <= 1; j++) {
                const [px, py] = jprng2(cx + i, cy + j);
                const realPx = px * this.size + (cx + i) * this.size;
                const realPy = py * this.size + (cy + j) * this.size;
                const dist = this.distance(x, y, realPx, realPy);
                if (dist < minDist) {
                    minDistSecond = minDist;
                    minDist = dist;
                } else if (dist < minDistSecond) {
                    minDistSecond = dist;
                }
            }
        }

        return Math.min(1, (minDistSecond - minDist) / this.size) < 0.02
            ? 1
            : 0;
    }

    distance(x1: number, y1: number, x2: number, y2: number) {
        const dx = x1 - x2;
        const dy = y1 - y2;
        return Math.sqrt(dx * dx + dy * dy);
    }

    clone(): Voronoi {
        return new Voronoi(
            this.size,
            this.filters.map((f) => f.clone())
        );
    }
}

export class VoronoiSquared extends Voronoi {
    distance(x1: number, y1: number, x2: number, y2: number) {
        const dx = x1 - x2;
        const dy = y1 - y2;
        return dx * dx + dy * dy;
    }

    clone(): VoronoiSquared {
        return new VoronoiSquared(
            this.size,
            this.filters.map((f) => f.clone())
        );
    }
}

export class VoronoiManhattan extends Voronoi {
    distance(x1: number, y1: number, x2: number, y2: number) {
        const dx = Math.abs(x1 - x2);
        const dy = Math.abs(y1 - y2);
        return dx + dy;
    }

    clone(): VoronoiManhattan {
        return new VoronoiManhattan(
            this.size,
            this.filters.map((f) => f.clone())
        );
    }
}

export class VoronoiCheby extends Voronoi {
    distance(x1: number, y1: number, x2: number, y2: number) {
        const dx = Math.abs(x1 - x2);
        const dy = Math.abs(y1 - y2);
        return Math.max(dx, dy);
    }

    clone(): VoronoiCheby {
        return new VoronoiCheby(
            this.size,
            this.filters.map((f) => f.clone())
        );
    }
}
export function withFilter(
    sig: Signal,
    x: number,
    y: number,
    filter: SignalFilter
) {
    return filter.process(x, y, sig.get(x, y));
}

/*
 * A base class whose job it is to give out (chunkSize*chunkSize) grids of tile is
 * */
abstract class TileLayer {
    /*
     * Expected to be a Grid that has run through a binary filter,
     * thet is, is only 0s and 1s.
     */
    mask: Signal;
    chunkSize = 4;
    srcMap: TiledMap;
    tile: number = 0;
    constructor(srcMap: TiledMap, mmask: Signal, tileId?: number) {
        this.srcMap = srcMap;
        this.mask = mmask.clone();
        this.tile = tileId || this.tile;
    }

    getXYTiles(x: number, y: number): Grid<number> {
        return DataGrid.createEmpty(4, 4, this.tile);
    }

    getTilesRect(x: number, y: number, w: number, h: number): Grid<number> {
        let out = DataGrid.createEmpty(
            w * this.chunkSize,
            h * this.chunkSize,
            0
        );
        for (let iy = 0; iy < h; iy++) {
            for (let ix = 0; ix < w; ix++) {
                out = addChunk(
                    out,
                    this.getXYTiles(ix + x, iy + y),
                    ix * this.chunkSize,
                    iy * this.chunkSize,
                    0
                ) as DataGrid<number>;
            }
        }
        return out;
    }
}

export class WangLayer extends TileLayer {
    wangSubArr: Grid<number>[];
    layerName: string;
    constructor(layerName: string, srcMap: TiledMap, mmask: Signal) {
        super(srcMap, mmask);
        this.layerName = layerName;
        const wangLayer: ITileLayer | undefined = this.srcMap
            .getLayers()
            .find(
                (item) => item.name === layerName && item.type === "tilelayer"
            ) as ITileLayer;
        if (!wangLayer) throw Error(`No layer "${this.layerName}" found`);

        this.wangSubArr = collectSubArr(
            this.chunkSize,
            this.chunkSize,
            new DataGrid(wangLayer.data, wangLayer.width)
        );
    }
    getXYTiles(x: number, y: number) {
        const wangval = getWangXY(
            DataGrid.fromGrid(this.mask.renderRect(x, y, 2, 2)),
            0,
            0,
            1
        );
        return this.wangSubArr[wangval];
    }
    clone() {
        return new WangLayer(this.layerName, this.srcMap, this.mask);
    }
}

/**
 * Systems are high level procedures for generating tiles and objects.
 * They only promise to provide layers through a getAllLayers method.
 */
interface System {
    getAllLayers: () => TileLayer | ObjectLayer[];
}
/*
 * A CliffSystem is essentially just a collection of Layer groups, where each group is
 * defined by a final mask around a certain segment of a Signal.  Where some n "segment"
 * is a final BinaryFilter set at the ratio n/divs.  `divs` are the total segments of the
 * System. Each segment holds an array of Layers that ascend towards higher final tile layers.
 *
 * The basic system is designed for a Perlin type signal to be used as the overall root signal,
 * and for the final layer of each segment to be a "cliff" type wang layer, any before that
 * fill in that section of cliff/altitude of the map.
 *
 */
export class CliffSystem implements System {
    private layers: WangLayer[][];
    extraLayers: WangLayer[];

    prefix: string;
    // baseWang = "dirt-hard";
    // secondaryWang = "grass";
    cliffWang: string = "cliffs";

    constructor(
        prefix: string,
        private signal: Signal,
        private srcMap: TiledMap,
        layers: WangLayer[][]
    ) {
        this.prefix = prefix;
        this.layers = layers;
        const trailSig = new VoronoiCheby(100);
        trailSig.filters.push(new EdgeFilter(trailSig));
        this.extraLayers = [new WangLayer("trail", srcMap, trailSig)];
    }

    getDivs() {
        return this.layers.length;
    }
    setSignal(sig: GenericSignal) {
        this.signal = sig;
    }

    /**
     * Get a signal which can act as a mask for this
     * altitude.  `n` must be < the.layers.length
     */
    private getDivMask(n: number) {
        const snapVal = n / this.layers.length;
        const sig = this.signal.clone();
        sig.filters = [...sig.filters, new BinaryFilter(snapVal)];
        return sig;
    }

    /*
     * Get the `n` layer group.  `n` must be within the cliff range, i.e. less
     * than the length of the layers array. This function takes care of the actual mask around an altitude segment,
     * as well as adding the cliffwang layer.
     */
    getLayerGroup(n: number): WangLayer[] | undefined {
        if (n >= this.layers.length) return undefined;
        let _layers = this.layers[n];
        // Reference the main cliff signal
        const _sig = this.signal.clone();
        // Alt mask for this layer
        const altMask = new SignalMaskFilter(1, this.getDivMask(n));
        if (!_sig.filters) _sig.filters = [];
        _layers.push(new WangLayer(this.cliffWang, this.srcMap, _sig));
        return _layers.map((lay) => {
            const tmpLayer = lay.clone();
            tmpLayer.mask.filters.push(altMask);
            return tmpLayer;
        });
    }

    getAllLayers() {
        return Array(this.getDivs())
            .fill(0)
            .flatMap((_, idx) => this.getLayerGroup(idx));
    }

    /**
     * Tile functions
     */

    getTileLayerGrids(
        layer: number,
        x: number,
        y: number,
        w: number,
        h: number
    ) {
        return this.getLayerGroup(layer)?.flatMap((item) =>
            item.getTilesRect(x, y, w, h)
        );
    }

    getAllTileLayerGrids(
        x: number,
        y: number,
        w: number,
        h: number
    ): Grid<number>[] {
        return Array(this.layers.length)
            .fill(0)
            .flatMap((_, idx) => this.getTileLayerGrids(idx, x, y, w, h))
            .filter((item) => item !== undefined) as Grid<number>[];
    }
}

/**
 * Functions for deserialization
 */

export function signalFromConfig(
    conf: SignalConfig
): GenericSignal | SignalFilter {
    const params = Object.fromEntries(conf.params);
    const filters =
        (conf.filters
            ?.map((f) => signalFromConfig(f))
            .filter((f) => GenericSignal.isSignal(f)) as SignalFilter[]) || [];
    switch (conf.type) {
        case "perlin":
            return new Perlin(params.freq, params.depth, params.seed, filters);

        case "fill":
            return new FillSignal();

        case "voronoi":
            return new Voronoi(params.size, filters);

        case "voronoi-squared":
            return new VoronoiSquared(params.size, filters);

        case "voronoi-man":
            return new VoronoiManhattan(params.size, filters);
        case "circle":
            return new CircleFilter(
                params.cx,
                params.cy,
                params.r,
                params.amount
            );
        case "binary":
            return new BinaryFilter(params.n);
    }
}

export function worldFromConfig(conf: WorldConfig, map: TiledMap) {
    const cliffSignal = signalFromConfig(conf.cliff_signal);
    if (!GenericSignal.isSignal(cliffSignal))
        throw Error("Issue with cliff signal configuration.");
    const layers = conf.cliffs.map((cliffgroup) => [
        new WangLayer(cliffgroup.base_texture, map, new FillSignal()),
        ...cliffgroup.layers.map((lay, idx) => {
            const laySignal = signalFromConfig(lay.signal);
            if (!GenericSignal.isSignal(laySignal))
                throw Error("Issue with layer signal. Layer  " + idx);
            return new WangLayer(lay.texture, map, laySignal);
        }),
    ]);
    return new CliffSystem("des", cliffSignal, map, layers);
}

export async function mapCliffPicture(
    cs: CliffSystem,
    ox: number,
    oy: number,
    w: number,
    h: number,
    ctx: CanvasRenderingContext2D,
    config: WorldConfig
) {
    const darkAmount = 100;
    let allLayers: WangLayer[] = [];
    const altMap: number[] = [];
    const alts = cs.getDivs();
    for (let i = alts - 1; i >= 0; i--) {
        const g = cs.getLayerGroup(i);

        if (g) {
            g[g.length - 1].mask.filters.push(
                new EdgeFilter(g[g.length - 1].mask)
            );
            for (let l = g.length - 1; l >= 0; l--) {
                allLayers.push(g[l]);
                altMap.push(i);
            }
        }
    }

    allLayers = [...cs.extraLayers, ...allLayers];
    const darken = new CachedVar((s: string) => {
        const [color, amount] = s.split("-");
        return Color(color).mix(Color("black"), parseFloat(amount)).hex();
    });
    for (let y = 0; y < h; y++) {
        for (let x = 0; x < w; x++) {
            for (let l in allLayers) {
                const wl = allLayers[l];
                let _color = config.colors[wl.layerName] || undefined;
                const val = wl.mask.get(x + ox, y + oy);
                if (!_color) {
                    const hex = (
                        "0" + Math.floor(val * 255).toString(16)
                    ).slice(-2);
                    _color = "#" + hex + hex + hex;
                }
                if (val === 1) {
                    ctx.fillStyle = darken.e(
                        [_color, `${0.17 * (alts - 1 - altMap[l])}`].join("-")
                    );

                    ctx.fillRect(x, y, 1, 1);
                    break;
                }
            }
        }
    }
}

class CachedVar<T> {
    vars: Record<string, T>;
    constructor(private func: (c: string) => T) {
        this.vars = {};
    }

    e(inp: string) {
        if (this.vars[inp]) return this.vars[inp];
        else {
            this.vars[inp] = this.func(inp);
            return this.vars[inp];
        }
    }
}

class ObjectSystem {}
