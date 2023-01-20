import { TiledMap } from "mapscripts/src/TiledMap";
import { perlin2d } from "mapscripts/src/perlin";
import {
    addChunk,
    clamp,
    collectSubArr,
    DataGrid,
    distance,
    getWangXY,
    Grid,
} from "mapscripts/src/utils";
import TiledRawJSON from "joegamelib/src/types/TiledRawJson";

export class WorldGenerator {
    signals: Signal[] = [];
    wangLayers: WangLayer[] = [];
    wangMap: TiledMap;
    constructor(tiledData: TiledRawJSON) {
        this.wangMap = new TiledMap(tiledData);
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
    id: number;
    ttype: SignalFilterTypes;
}

export class CircleFilter implements SignalFilter {
    ttype = SignalFilterTypes.circle;
    id: number;
    constructor(
        private cx: number,
        private cy: number,
        private r: number,
        private amount: number,
        id: number
    ) {
        this.id = id;
    }
    process(x: number, y: number, val: number) {
        const dist = distance(x, y, this.cx, this.cy);
        const fact = dist < this.r ? 1 + this.amount * (1 - dist / this.r) : 1;
        return clamp(fact * val, 0.07, 0.94);
    }
}
export class SnapFilter implements SignalFilter {
    ttype = SignalFilterTypes.snap;
    id: number;
    constructor(private snaps: number, id: number) {
        this.id = id;
    }
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
}

export class BinaryFilter implements SignalFilter {
    ttype = SignalFilterTypes.binary;
    id: number;
    constructor(private n: number, id: number) {
        this.id = id;
    }
    process(_x: number, _y: number, val: number) {
        if (val >= this.n) return 1;
        else return 0;
    }
}

export class SignalMaskFilter implements SignalFilter {
    ttype = SignalFilterTypes.signalmask;
    id: number;
    constructor(private n: number, private sig: Signal, id: number) {
        this.id = id;
        this.sig.filters.push(new BinaryFilter(n, 0));
    }
    process(x: number, y: number, val: number) {
        if (this.sig.get(x, y) === 1) return val;
        else return 0;
    }
}
type Signal = GenericSignal;
class GenericSignal {
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
        cb = (_x: number, _y: number, v: number) => v
    ): number[][] {
        return Array(h)
            .fill(0)
            .map((_row, y) =>
                Array(w)
                    .fill(0)
                    .map((_item, x) => cb(x, y, this.get(x, y)))
            );
    }

    async asyncRender(
        w: number,
        h: number,
        cb = (_x: number, _y: number, v: number) => v
    ) {
        return new Promise<number[][]>((res) => res(this.render(w, h, cb)));
    }

    async renderToContext(w: number, h: number, ctx: CanvasRenderingContext2D) {
        return this.asyncRender(w, h, (x, y, val) => {
            const hex = ("0" + Math.floor(val * 255).toString(16)).slice(-2);
            const color = "#" + hex + hex + hex;
            ctx.fillStyle = color;
            ctx.fillRect(x, y, 1, 1);
            return val;
        });
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

    private applySignalFilters(
        x: number,
        y: number,
        val: number,
        filters?: SignalFilter[]
    ): number {
        if (filters) {
            if (filters.length > 0) {
                return this.applySignalFilters(
                    x,
                    y,
                    filters[0].process(x, y, val),
                    filters.slice(1)
                );
            } else return val;
        } else if (this.filters.length > 0)
            return this.applySignalFilters(
                x,
                y,
                this.filters[0].process(x, y, val),
                this.filters.slice(1)
            );
        else return val;
    }

    getBaseValue(_x: number, _y: number) {
        return 0;
    }
    clone() {
        return new GenericSignal(this.filters);
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
        return new Perlin(this.freq, this.depth, this.seed, this.filters);
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

export class WangLayer {
    wangSize = 4;
    wangSubArr: Grid<number>[];
    mask: Signal;
    constructor(
        private layerName: string,
        private wangMap: TiledMap,
        mmask: Signal
    ) {
        const wangLayer = this.wangMap
            .getLayers()
            .find((item) => item.name === this.layerName);
        if (!wangLayer) throw Error(`No layer "${this.layerName}" found`);

        this.wangSubArr = collectSubArr(
            this.wangSize,
            this.wangSize,
            new DataGrid(wangLayer.data, wangLayer.width)
        );

        // ensure signal is binary mask
        this.mask = mmask.clone();
        this.mask.filters.push(new BinaryFilter(0.5, 420));
    }

    getXYTiles(x: number, y: number) {
        const wangval = getWangXY(
            DataGrid.fromGrid(this.mask.renderRect(x, y, 2, 2)),
            x,
            y,
            1
        );
        return this.wangSubArr[wangval];
    }

    getTilesRect(x: number, y: number, w: number, h: number) {
        const out = DataGrid.createEmpty(
            w * this.wangSize,
            h * this.wangSize,
            0
        );
        for (let iy = y; iy < y + h; iy++) {
            for (let ix = x; ix < x + w; ix++) {
                addChunk(
                    out,
                    this.getXYTiles(ix, iy),
                    ix * this.wangSize,
                    iy * this.wangSize,
                    0
                );
            }
        }
        return out;
    }
}

export class CliffSystem {
    private layers: WangLayer[];
    prefix: string;

    constructor(
        prefix: string,
        private signal: Signal,
        private divs: number,
        private wangmap: TiledMap,
        private wangLayerName: string
    ) {
        this.layers = this.genLayers();
        this.prefix = prefix;
    }

    setDivs(n: number) {
        this.divs = n;
    }

    getDivs() {
        return this.divs;
    }

    getAltitudeLayer(n: number) {
        if (n >= this.layers.length) {
            console.log(`Altitude ${n} exceeds the signals snaps, CliffSystem`);
        }
        return this.layers[n];
    }

    private genLayers() {
        return Array(this.divs)
            .fill(0)
            .map((_, idx) => {
                return new WangLayer(
                    this.wangLayerName,
                    this.wangmap,
                    this.getDivMask(idx)
                );
            });
    }

    private getDivMask(n: number) {
        const snapVal = n / this.divs;
        const sig = this.signal.clone();
        sig.filters.push(new BinaryFilter(snapVal, 1234));
        return sig;
    }
}

export class CliffTerrainSystem {
    constructor(private cliffSystem: CliffSystem, private wangmap: TiledMap) {}
}
