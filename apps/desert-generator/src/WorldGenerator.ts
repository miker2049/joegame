import { perlin2d } from "mapscripts/src/perlin";
import { clamp, distance } from "mapscripts/src/utils";

export class WorldGenerator {
    signals: Signal[] = [];
    constructor(sigs: Signal[]) {
        this.signals = sigs;
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

abstract class Signal {
    filters: SignalFilter[];

    constructor() {
        this.filters = [];
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

    private applySignalFilters(
        x: number,
        y: number,
        val: number,
        filters?: typeof this.filters
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
}

export class Perlin extends Signal {
    freq: number;
    depth: number;
    seed: number;
    constructor(freq: number, depth: number, seed: number) {
        super();
        this.freq = freq;
        this.depth = depth;
        this.seed = seed;
    }
    getBaseValue(x: number, y: number) {
        return perlin2d(x, y, this.freq, this.depth, this.seed);
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
