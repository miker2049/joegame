import { TiledMap } from "./TiledMap";
import { perlin2d } from "./perlin";
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
import { hexToRGB, hsvToRgb, rgbToHex, rgbToHsv } from "./color";

type SignalConfig = {
    type: "perlin" | "fill" | "circle" | "binary";
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
            12,
            this.wangMap
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
    constructor(private n: number, private sig: Signal) {
        this.sig.filters.push(new BinaryFilter(n));
    }
    process(x: number, y: number, val: number) {
        if (this.sig.get(x, y) === 1) return val;
        else return 0;
    }
    clone() {
        return new SignalMaskFilter(this.n, this.sig.clone());
    }
}

export type Signal = GenericSignal;

export class GenericSignal {
    filters: SignalFilter[];

    constructor(f?: SignalFilter[]) {
        this.filters = f || [];
    }

    get(x: number, y: number): number {
        const out = this.applySignalFilters(x, y, this.getBaseValue(x, y));
        // if (!out) throw Error(`Issue with signal ${x} ${y}`);
        return out;
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
                    // modify value against the HSvalue
                    // const hsv = rgbToHsv(hexToRGB(color));
                    // hsv.v = Math.min(Math.max(100 * val, 60), 40);
                    _color = val > 0.5 ? color + "ff" : "#00000000";
                }
                if (val > 0.5) {
                    // console.log(_color);
                    ctx.fillStyle = _color;
                    ctx.fillRect(x, y, 1, 1);
                }
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
export class CliffSystem {
    private layers: WangLayer[][];

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
    getLayerGroup(n: number) {
        if (n >= this.layers.length) return undefined;
        const _layers = this.layers[n];
        // Reference the main cliff signal
        const _sig = this.signal.clone();
        // Alt mask for this layer
        const altMask = new SignalMaskFilter(1, this.getDivMask(n));
        if (!_sig.filters) _sig.filters = [];
        _sig.filters.push(altMask);
        // _layers.push(new WangLayer(this.cliffWang, this.srcMap, _sig));
        return this.layers[n].map((lay) => {
            const tmpLayer = lay.clone();
            tmpLayer.mask.filters.push(altMask);
            return tmpLayer;
        });
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

function signalFromConfig(conf: SignalConfig) {
    const params = Object.fromEntries(conf.params);
    switch (conf.type) {
        case "perlin":
            return new Perlin(
                params.freq,
                params.depth,
                params.seed,
                conf.filters?.map((f) => signalFromConfig(f)) || []
            );
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
    const layers = conf.cliffs.map((cliffgroup) =>
        cliffgroup.layers.map(
            (lay) =>
                new WangLayer(lay.texture, map, signalFromConfig(lay.signal))
        )
    );
    return new CliffSystem("des", cliffSignal, map, layers);
}

export function mapCliffPicture(
    cs: CliffSystem,
    ox: number,
    oy: number,
    w: number,
    h: number,
    ctx: CanvasRenderingContext2D,
    config: WorldConfig
) {
    const alts = cs.getDivs();
    for (let i = 0; i < alts; i++) {
        const g = cs.getLayerGroup(i);
        if (g)
            for (let l = 0; l < g.length; l++) {
                console.log(g[l].layerName, i, config.colors[g[l].layerName]);
                g[l].mask.renderToContext(
                    w,
                    h,
                    ctx,
                    config.colors[g[l].layerName] || undefined,
                    ox,
                    oy
                );
            }
    }
}
