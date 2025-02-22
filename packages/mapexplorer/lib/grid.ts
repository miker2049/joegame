export function indexToCoords(n: number, w: number): [number, number] {
    return [n % w, Math.floor(n / w)];
}

export function coordsToIndex(x: number, y: number, width: number): number {
    return y * width + x;
}
export function clamp(value: number, min: number, max: number) {
    return Math.max(min, Math.min(max, value));
}

export enum StyleDir {
    right,
    left,
    top,
    bottom,
}

export class Grid<T> {
    width: number;

    constructor(
        private data: T[],
        width: number,
    ) {
        this.width = width;
    }
    at(x: number, y: number): T {
        if (x >= this.width || y >= this.height()) return undefined as T;
        return this.data[this.i(x, y)];
    }
    i(x: number, y: number): number {
        return coordsToIndex(x, y, this.width);
    }
    setVal(x: number, y: number, val: T): boolean | undefined {
        if (val === undefined) return undefined;
        this.data[coordsToIndex(x, y, this.width)] = val;
        return true;
    }
    row(y: number) {
        const offset = y * this.width;
        return this.data.slice(offset, offset + this.width);
    }
    height() {
        return Math.floor(this.data.length / this.width);
    }
    clone() {
        return new Grid<T>([...this.data], this.width);
    }
    getData(): T[] {
        return this.data;
    }
    get2dArr(): T[][] {
        return unflat<T>(this.data, this.width);
    }
    print(): string {
        return this.get2dArr()
            .map((item) => item.map((num) => String(num).padStart(4, " ")))
            .map((item) => item.join("  "))
            .join("\n");
    }

    isSame(grid: Grid<unknown>): boolean {
        const _grid = grid.getData();
        const thisData = this.getData();
        let out = true;
        thisData.forEach((dd, i) => {
            if (dd != _grid[i]) {
                out = false;
            }
        });
        return out;
    }

    isEmpty(emptyEntity: T | undefined): boolean {
        let out = true;
        for (let y = 0; y < this.height(); y++) {
            for (let x = 0; x < this.width; x++) {
                const val = this.at(x, y);
                if (val != emptyEntity) {
                    out = false;
                    break;
                }
            }
        }
        return out;
    }

    pad(am: number, val: T, dir = StyleDir.bottom) {
        switch (dir) {
            case StyleDir.bottom:
                this.padRow(am, val, true);
                break;
            case StyleDir.top:
                this.padRow(am, val, false);
                break;
            case StyleDir.left:
                this.padCol(am, val, true);
                break;
            case StyleDir.right:
                this.padCol(am, val, false);
                break;
        }
    }

    private padRow(am: number, val: T, appendBottom: boolean) {
        const newRows = Array(am).fill(Array(this.width).fill(val));
        const tmp = addChunk(
            this,
            Grid.fromGrid(newRows),
            0,
            appendBottom ? this.height() : -am,
            val,
        );
        this.data = tmp.getData();
    }
    private padCol(am: number, val: T, appendLeft: boolean) {
        const newCols = Array(this.height()).fill(Array(am).fill(val));
        const tmp = addChunk(
            this,
            Grid.fromGrid(newCols),
            appendLeft ? -am : this.width,
            0,
            val,
        );
        this.data = tmp.getData();
    }

    getCenter(): [number, number] {
        return [Math.floor(this.width / 2), Math.floor(this.height() / 2)];
    }

    entries(test?: (l: T) => boolean): Array<T & { x: number; y: number }> {
        const out = new Array<T & { x: number; y: number }>();
        iterateGrid(this, (x, y, v) => {
            if (!test) out.push({ ...v, x, y });
            else if (test(v)) out.push({ ...v, x, y });
        });
        return out;
    }

    static fromGrid<T = number | undefined>(grid: T[][], width?: number) {
        const width_ = width ?? grid[0].length;
        return new Grid<T>(grid.flat(), width_);
    }
    static createEmpty<T>(width: number, height: number, def: T) {
        const data = Array(width * height).fill(def);
        return new Grid(data, width);
    }
}

/*
 * Where g is a grid and queries is a list of things to check for based on getting a hash
 */

// export function checkGridForMatches<T>(
//     g: Grid<T>,
//     queries: T[],
//     checkValue: T,
// ) {
//     const height = g.height();
//     for (let y = 1; y < height - 1; y++) {
//         for (let x = 1; x < g.width - 1; x++) {
//             const m = getMask(getSubArr<T>(x - 1, y - 1, 3, 3, g), checkValue);
//         }
//     }
// }

/*
 * Takes a 2d array and returns sub array of it
 */
export function getSubArr<T>(
    x: number,
    y: number,
    width: number,
    height: number,
    arr: Grid<T>,
): Grid<T> {
    const out: Grid<T> = new Grid<T>([], width);
    for (let i = 0; i < height; i++) {
        for (let j = 0; j < width; j++) {
            const found = arr.at(j + x, i + y);
            out.setVal(j, i, found);
        }
    }
    return out;
}

/*
 * Takes a binary mask and returns an array of rectangles, [x, y, length, width].
 * It has to be an array with the largest rects.
 *
 * 000xx
 * 000xx
 * 000x0
 *
 * This needs to produce 2
 */

/*
 * Takes an array and width, and returns 2d array
 */
export function unflat<T>(g: T[], w: number): T[][] {
    let out: T[][] = [];
    const dl = g.length;
    const rows = Math.floor(dl / w);
    for (let y = 0; y < rows; y++) {
        out[y] = [];
        for (let x = 0; x < w; x++) {
            out[y][x] = g[coordsToIndex(x, y, w)];
        }
    }
    return out;
}

/*
 * Takes an array of bits and returns the hex equivalent
 */
export function binaryArrayToHex(arr: number[]): string {
    const binaryString = arr.map((num) => (num === 0 ? 0 : 1)).join("");
    return parseInt(binaryString, 2).toString(16);
}

/*
 * collecting subarrays from one big array, given a width & height you want the subs to be
 */
export function collectSubArr<T>(
    width: number,
    height: number,
    arr: Grid<T>,
): Grid<T>[] {
    const input_height = arr.height();
    const input_width = arr.width;
    let out: Grid<T>[] = [];
    if (width > input_width || height > input_height) return out;
    for (let i = 0; i < input_height; i += height) {
        for (let j = 0; j < input_width; j += width) {
            out.push(getSubArr<T>(j, i, width, height, arr));
        }
    }
    return out;
}

export function encodeGrid<T>(arr: Grid<T>, valCheck: T) {
    let out: number = 0b0;
    iterateGrid(arr, (_x, _y, val) => {
        out = val === valCheck ? (out << 1) | 1 : (out << 1) | 0;
    });
    return out;
}

export function gridFromRegionCode<T>(code: number, g: Grid<T>): Grid<T> {
    const size = g.width * g.height();
    const pad = Array(size).fill("0").join("");
    let mask = Number(code).toString(2);
    mask = (pad + mask).slice(-1 * size);
    const maskArr = mask.split("").map((i) => parseInt(i));
    const maskGrid = Grid.fromGrid(unflat(maskArr, g.width));
    let [minX, minY, maxX, maxY] = [Infinity, Infinity, 0, 0];
    iterateGrid(maskGrid, (x, y, v) => {
        if (v === 1) {
            minX = Math.min(x, minX);
            minY = Math.min(y, minY);
            maxX = Math.max(x, maxX);
            maxY = Math.max(y, maxY);
        }
    });
    let out = getSubArr(minX, minY, maxX - minX + 1, maxY - minY + 1, g);
    return out;
}

export function getGrowComponents<T>(
    row: number,
    g: Grid<T>,
): [Grid<T>, Grid<T>, Grid<T>] {
    const top = getSubArr<T>(0, 0, g.width, row, g);
    const filler = getSubArr<T>(0, row, g.width, 1, g);
    const base = getSubArr<T>(0, row + 1, g.width, g.height() - (row + 1), g);
    return [base, filler, top];
}
/*
 * Grows a Grid n vertically, given a row index to repeat as filler.
 */
export function growGridVertical<T>(
    n: number,
    row: number,
    g: Grid<T>,
    def: T,
): Grid<T> {
    let [base, filler, top] = getGrowComponents(row, g);
    if (n > 0) {
        for (let _ in Array(n).fill(0)) {
            top = addChunk(top, filler, 0, top.height(), def);
        }
    }
    top = addChunk(top, base, 0, top.height(), def);
    return top;
}

export function findInGrid<T>(
    patterns: Grid<T> | Grid<T>[],
    base: Grid<T>,
): { x: number; y: number }[][] {
    const _patterns = Array.isArray(patterns) ? patterns : [patterns];
    const out: { x: number; y: number }[][] = new Array(_patterns.length)
        .fill(0)
        .map((_) => []);
    iterateGrid<T>(base, (bx, by, _bv) => {
        for (let pattern in _patterns) {
            let matching = true;
            iterateGrid<T>(_patterns[pattern], (px, py, pv) => {
                if (pv != base.at(px + bx, py + by)) matching = false;
            });
            if (matching) out[pattern].push({ x: bx, y: by });
        }
    });
    return out;
}

export function findAndReplaceAllGrid<T>(
    patterns: Grid<T> | Grid<T>[],
    replacements: Grid<T> | Grid<T>[],
    base: Grid<T>,
) {
    const _patterns = Array.isArray(patterns) ? patterns : [patterns];
    const _replacements = Array.isArray(replacements)
        ? replacements
        : [replacements];
    const res = findInGrid(_patterns, base);
    for (let pattern in res) {
        for (let item in res[pattern]) {
            base = addChunk(
                base,
                _replacements[pattern],
                res[pattern][item].x,
                res[pattern][item].y,
                0 as unknown as T,
            ); // >:(
        }
    }
    return base;
}

export function normalizeGrid(grid: Grid<number>) {
    const [min, max] = getMinMaxGrid(grid);
    return mapGrid<number, number>(grid, (_x, _y, val) => {
        return (val - min) / (max - min);
    });
}

/*
 * Returns a tuple [min, max] where min and max are the
 * minimum and maximum value repectively of the given grid.
 */
export function getMinMaxGrid(grid: Grid<number>): [number, number] {
    let min = Infinity;
    let max = 0;
    iterateGrid(grid, (_x, _y, val) => {
        min = Math.min(val, min);
        max = Math.max(val, max);
    });
    return [min, max];
}

export function snapNormalGrid(
    grid: Grid<number>,
    range: number,
    reverse: boolean = false,
) {
    return mapGrid<number, number>(grid, (_x, _y, val) => {
        val = Math.max(val, 0);
        val = Math.min(val, 1);
        let out = 0;
        const segs = 1 / range;
        for (let i = 0; i < range; i++) {
            if (val >= i * segs && val <= i * segs + segs) {
                out = reverse ? range - (i + 1) : i;
            }
        }
        return out;
    });
}

export function iterateGrid<T>(
    arr: Grid<T>,
    cb: (x: number, y: number, value: T) => void,
) {
    for (let y = 0; y < arr.height(); y++) {
        for (let x = 0; x < arr.width; x++) {
            const val = arr.at(x, y);
            cb(x, y, val!);
        }
    }
}

export function mapGrid<T, R>(
    arr: Grid<T>,
    cb: (x: number, y: number, value: T) => T | undefined,
): Grid<R> {
    const out = Grid.createEmpty(arr.width, arr.height(), 0);
    iterateGrid(arr, (x, y, value) => out.setVal(x, y, cb(x, y, value)));
    return out;
}

/*
 * Consol
 */
export function consolidateGrids<T>(gs: Grid<T>[], max: number) {
    const out: Grid<T>[] = Array(max)
        .fill(0)
        .map((_) => Grid.createEmpty(gs[0].width, gs[0].height(), 0));
    iterateGrid(gs[0], (x, y, _v) => {
        let thisI = 0;
        gs.forEach((grid) => {
            const val = grid.at(x, y);
            if (val ?? 0 > 0) {
                out[thisI].setVal(x, y, val);
                thisI = thisI === max - 1 ? 0 : thisI + 1;
            }
        });
    });
    return out;
}

export function getMaxXofGrid<T>(g: T[][]): number {
    // for combatability
    return g.map((v) => v.length).reduce((p, c) => Math.max(p, c));
    // return g.width
}

export function makeEmptyGrid<T>(w: number, h: number, v: T): Grid<T> {
    return new Grid<T>(Array(w * h).fill(v), w);
    // return new Array(h).fill(0).map(_ => new Array(w).fill(v))
}

export function gridCopy<T>(arr: T[][]): T[][] {
    let out: T[][] = [];
    arr.forEach((item, i) => (out[i] = [...item]));
    return out;
}

/*
 * Takes a base grid and puts an overlay (ol) ontop with offset. A default value (def) is given as well.
 *
 * If a negative offset is used, the map will grow to allow it.
 */
export function attachTileChunks<T>(
    base: Grid<T>,
    ol: Grid<T>,
    xo: number,
    yo: number,
    def: T,
): Grid<T> {
    const height = Math.max(base.height(), ol.height() + yo);
    const width = Math.max(base.width, ol.width + xo);
    let out = makeEmptyGrid(width, height, def);
    iterateGrid(out, (x: number, y: number, _v: T) => {
        if (base.at(x, y) != undefined) {
            out.setVal(x, y, base.at(x, y)!);
        }
        const olX = x - xo;
        const olY = y - yo;
        if (olX >= 0 && olY >= 0) {
            out.setVal(x, y, ol.at(olX, olY)!);
        }
    });
    return out;
}

/*
 * Quickly injects chunk ol into base and returns it.  Doesn't check for growth,
 * omits ones not landing on base.
 */
export function injectChunk<T>(
    base: Grid<T>,
    ol: Grid<T>,
    xo: number,
    yo: number,
): Grid<T> {
    iterateGrid(ol, (x, y, val) => {
        if (base.at(x + xo, y + yo) != undefined) {
            base.setVal(x + xo, y + yo, val);
        }
    });
    return base;
}

/*
 * Dynamically pick between inject and attachTileChunk based on needd
 */
export function addChunk<T>(
    base: Grid<T>,
    ol: Grid<T>,
    xo: number,
    yo: number,
    def: T,
): Grid<T> {
    if (!base) throw Error("base is not defined");
    if (!ol) throw Error("overlay layer is not defined");
    if (xo < 0 || yo < 0) {
        const width =
            xo < 0
                ? Math.max(Math.abs(xo) + base.width, ol.width)
                : Math.max(base.width, xo + ol.width);
        const height =
            yo < 0
                ? Math.max(Math.abs(yo) + base.height(), ol.height())
                : Math.max(base.height(), yo + ol.height());
        const baseXo = xo < 0 ? Math.abs(xo) : 0;
        const baseYo = yo < 0 ? Math.abs(yo) : 0;
        const olXo = xo < 0 ? 0 : xo;
        const olYo = yo < 0 ? 0 : yo;
        let out: Grid<T> = Grid.createEmpty(width, height, def);
        out = addChunk(out, base, baseXo, baseYo, def);
        out = addChunk(out, ol, olXo, olYo, def);
        return out;
    } else if (base.height() < ol.height() + yo || base.width < ol.width + xo) {
        return attachTileChunks<T>(base, ol, xo, yo, def);
    } else {
        return injectChunk<T>(base, ol, xo, yo);
    }
}

/*
 * base:
 * x,x,x
 * x,x,x
 *
 * ol:
 * y,y
 * y,y
 *
 * xo = 1
 * yo = -2
 * Should equal:
 * 0,y,y
 * 0,y,y
 * x,x,x
 * x,x,x
 *
 * where its fixed at +2 y, but 0 x
 */

export function printGrid<T>(g: Grid<T>): string {
    return g.print();
}

export function distance(x1: number, y1: number, x2: number, y2: number) {
    return Math.sqrt(Math.pow(x2 - x1, 2) + Math.pow(y2 - y1, 2));
}

export function distortBubble(
    arr: number[][],
    x: number,
    y: number,
    r: number,
    amount: number,
) {
    const g = new Grid(
        arr.flatMap((i) => i),
        arr[0].length,
    );
    return mapGrid<number, number>(g, (cx, cy, cv) => {
        const dist = distance(x, y, cx, cy);
        const fact = dist < r ? 1 + amount * (1 - dist / r) : 1;
        return clamp(fact * cv, 0, 1);
    });
}

/*
 * Apply distort bubble defined with x,y,r to val at cx,cy
 */
export function applyDistortBubble({
    cx,
    cy,
    val,
    x,
    y,
    r,
    amount,
}: {
    cx: number;
    cy: number;
    val: number;
    x: number;
    y: number;
    r: number;
    amount: number;
}) {
    const dist = distance(x, y, cx, cy);
    const fact = dist < r ? 1 + amount * (1 - dist / r) : 1;
    return clamp(fact * val, 0.07, 0.94);
}

/*
 * ty, https://stackoverflow.com/a/55671924 and robot
 */
export function weightedChoose<T>(arr: T[], weights: number[], rval?: number) {
    rval = rval || Math.random();
    rval = Math.max(0, Math.min(rval, 0.999999999));
    // Compute the total weight of all items
    const totalWeight = weights.reduce((a, b) => a + b, 0);

    // Generate a random number between 0 and totalWeight
    const randomNumber = rval * totalWeight;

    // Iterate through the items and weights, accumulating the weights until
    // the sum exceeds the random number. Return the corresponding item.
    let weightSum = 0;
    for (let i = 0; i < arr.length; i++) {
        weightSum += weights[i];
        if (randomNumber < weightSum) {
            return arr[i];
        }
    }

    // This should never happen, but just in case
    throw new Error("weightedChoose: unable to choose an item");
}

/*
 * [0,0,0,0]
 * [0,1,1,0]
 * [0,1,1,0]
 * [0,0,0,0]
 */

// export function multiplyGrids(a: Grid<number>, b: Grid<number>) {
//     const am = new Matrix(a.get2dArr() as number[][]);
//     const bm = new Matrix(b.get2dArr() as number[][]);
//     return Grid.fromGrid(am.times(bm).matrix);
// }

export function scaleGrid<T>(inp: Grid<T>, scale: number) {
    const outWidth = Math.floor(inp.width * scale);
    const outHeight = Math.floor(inp.height() * scale);
    if (outWidth <= 0 || outHeight <= 0) throw Error("scale grid failed");
    const out = Grid.createEmpty(outWidth, outHeight, 0);
    return mapGrid<T, number>(out, (x, y, _v) => {
        return scaledXY(inp, scale, x, y);
    });
}

/*
 * Get the val of Grid inp at (x,y) if it was scaled by scale
 */
export function scaledXY<T>(inp: Grid<T>, scale: number, x: number, y: number) {
    return inp.at(
        Math.max(Math.floor(x / scale), 0),
        Math.max(Math.floor(y / scale), 0),
    );
}

export function getWangXY(
    grid: Grid<number>,
    x: number,
    y: number,
    check: number,
) {
    // The overall procedure relies on scaling by 2, and offseting by one
    // When we request the wang tile at (0,0), it is made up of the 2xGrid coords (1,1),(2,1),(1,2),(2,2)
    const xo = x + 1,
        yo = y + 1;
    const quad = Grid.fromGrid([
        [scaledXY(grid, 2, xo, yo), scaledXY(grid, 2, xo + 1, yo)],
        [scaledXY(grid, 2, xo, yo + 1), scaledXY(grid, 2, xo + 1, yo + 1)],
    ]);
    // console.log(xo, yo);
    return calcWangVal(0, 0, quad, check);
}

/*
 * takes a grid of presumably pixels, and checks for check vals in 2x2 chunks, in the corners,
 * http://www.cr31.co.uk/stagecast/wang/2corn.html
 * assinging a bitwise number (0-16).
 * The returned grid is half the size of the input
 */
export function pixelsToWang2Corners(
    grid: Grid<number>,
    check: number,
): Grid<number> {
    // console.log(grid.print());
    const gheight = grid.height();
    const out = Grid.createEmpty(grid.width / 2, gheight / 2, 0);
    for (let y = 1; y < grid.width - 1; y += 2) {
        for (let x = 1; x < gheight - 1; x += 2) {
            out.setVal(
                (x - 1) / 2,
                (y - 1) / 2,
                calcWangVal(x, y, grid, check),
            );
        }
    }
    return out;
}

export function calcWangVal(
    x: number,
    y: number,
    grid: Grid<number>,
    check: number,
) {
    let n = 0;
    grid.at(x, y) == check ? (n |= 0b1000) : undefined;
    grid.at(x + 1, y) == check ? (n |= 0b1) : undefined;
    grid.at(x, y + 1) == check ? (n |= 0b100) : undefined;
    grid.at(x + 1, y + 1) == check ? (n |= 0b10) : undefined;
    return n;
}

export function addTilesFromWang(
    wangt: Grid<number>,
    stamps: Grid<number>[],
    stampSize: number,
): Grid<number> {
    const out = Grid.createEmpty(
        wangt.width * stampSize,
        wangt.height() * stampSize,
        0,
    );
    iterateGrid(wangt, (x, y, val) => {
        const chunk = stamps[val];
        if (!chunk) console.log("no chunk at index " + wangt.at(x, y));
        else addChunk(out, chunk, x * stampSize, y * stampSize, 0);
    });
    return out;
}

export function pathBasename(path: string) {
    return path.split("/").reverse()[0];
}

/**
 * TileStacks is used by the saturator.  Can "stack" an arbritray amound of tiles per coord
 * and then return them as normalized Grids for use in TiledMaps
 */
export class TileStacks {
    grid: Grid<number[]>;
    constructor(w: number, h: number) {
        this.grid = Grid.createEmpty<(number | undefined)[]>(w, h, []);
    }
    push(x: number, y: number, v: number) {
        if (x > this.grid.width || y > this.grid.height()) return;
        let arr = this.grid.at(x, y);
        if (!arr) arr = [];
        arr.push(v);
        this.grid.setVal(x, y, arr);
    }
    at(x: number, y: number) {
        return this.grid.at(x, y);
    }

    addChunk(g: Grid<number>, xo: number, yo: number) {
        iterateGrid(g, (x, y, val) => {
            this.push(x + xo, y + yo, val);
        });
        return this.grid;
    }

    split(func: (n: number) => boolean) {
        const w = this.grid.width,
            h = this.grid.height();
        const a = new TileStacks(w, h),
            b = new TileStacks(w, h);

        iterateGrid(this.grid, (x, y, val) => {
            if (val)
                val.forEach((v) =>
                    func(v) ? a.push(x, y, v) : b.push(x, y, v),
                );
        });
        return [a, b];
    }
    isEmpty(): boolean {
        return this.grid.isEmpty(undefined);
    }
    private maxStack() {
        let max = 0;
        iterateGrid(this.grid, (_x, _y, val) => {
            max = Math.max(max, val ? val.length : 0);
        });
        return max;
    }
    getLgs() {
        const out: Grid<number>[] = [];
        for (let i = 0; i < this.maxStack(); i++) {
            out[i] = Grid.createEmpty(this.grid.width, this.grid.height(), 0);
        }
        iterateGrid(this.grid, (x, y, val) => {
            if (val) val.forEach((v, idx) => out[idx].setVal(x, y, v));
        });
        return out;
    }
}

/// for discrete limited objects
// tweeters, books, animals, music, and such
/**
 * We are getting the boxes that are the # signs here
 *
 *  . . . . . . . . .
 *  . . . . . . . . .
 *  . . . # . . . . .
 *  . . . . . . . . .
 *  . . . . . . . . .
 *
 *  . . . . . . . . .
 *  . . # # # . . . .
 *  . . # * # . . . .
 *  . . # # # . . . .
 *  . . . . . . . . .
 *
 *  . # # # # # . . .
 *  . # . . . # . . .
 *  . # . * . # . . .
 *  . # . . . # . . .
 *  . # # # # # . . .
 *
 *  . # # # # # # # .
 *  . # . . . . . # .
 *  . # . . . . . # .
 *  . # . . * . . # .
 *  . # . . . . . # .
 *  . # . . . . . # .
 *  . # # # # # # # .
 *
 */
export function getStepBoxEdge(n: number): [number, number][] {
    if (n === 0) return [[0, 0]];
    else {
        const out: [number, number][] = [];
        // the size of a top/bottom row is always n*2+1
        for (let i = 0; i < n * 2 + 1; i++) {
            const x = i - n;
            // top row
            out.push([x, -n]);
            // bottom
            out.push([x, n]);
            // if we arent at the common corner, go ahead and add the cols
            if (i != 0 && i != n * 2) {
                out.push([-n, x]);
                out.push([n, x]);
            }
        }
        return out;
    }
}

/**
 * Get's the step of this object, which is prereq for finding the final box
 *
 * @argument s -- the saturation in effect here
 * @argument idx -- the index of this objec
 */
export function getObjectStep(s: number, idx: number) {
    idx = Math.abs(idx);
    if (idx < s || s === Infinity) return 0;
    let curr = 1;
    while (true) {
        if (idx < s * (8 * curr)) {
            return curr;
        } else curr += 1;
    }
}

/**
 * A given idx and saturation input will
 * return the quad this idx should be randomly placed in.
 *
 * While this is deterministic, it is tied to the way getStepBoxEdge is implemented,
 * and specifically the order of the quads it returns, which is kind arbritrary rn.
 *
 * This will return a normalized coordinate to the quad, i.e., as if our origin is at [0,0]
 */
export function getQuadForObject(s: number, idx: number): [number, number] {
    const step = getObjectStep(s, idx);
    const boxes = getStepBoxEdge(step);
    return boxes[idx % boxes.length];
}

/**
 * Map value from [inMin,inMax] range to [outMin,outMax]
 */
export function mapRange(
    value: number,
    inMin: number,
    inMax: number,
    outMin: number,
    outMax: number,
): number {
    return ((value - inMin) * (outMax - outMin)) / (inMax - inMin) + outMin;
}

export function cullCoordinates<
    T extends { x: number; y: number; priority: number },
>(coordinates: T[], n: number): T[] {
    const culled: T[] = [];

    // Loop through each coordinate and compare to all others
    for (let i = 0; i < coordinates.length; i++) {
        let keep = true;
        for (let j = 0; j < coordinates.length; j++) {
            if (i !== j) {
                const distance = Math.sqrt(
                    Math.pow(coordinates[i].x - coordinates[j].x, 2) +
                        Math.pow(coordinates[i].y - coordinates[j].y, 2),
                );
                if (distance < n) {
                    // Remove the coordinate with lower priority
                    if (coordinates[i].priority < coordinates[j].priority) {
                        keep = false;
                    } else if (culled.includes(coordinates[j])) {
                        culled.splice(culled.indexOf(coordinates[j]), 1);
                    }
                }
            }
        }
        // Add coordinate to culled if it has not been removed
        if (keep) {
            culled.push(coordinates[i]);
        }
    }

    return culled;
}

/**
 * Simple memiozation utility
 */
export class CachedVar<T> {
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
