import TiledRawJSON, { ILayer } from '../../src/types/TiledRawJson';
import { readFile } from 'fs/promises'
import { coordsToIndex } from '../../src/utils/indexedCoords'
import { TiledMap } from './TiledMap';


export interface Grid<T = number> {
    at: (x: number, y: number) => T | undefined
    setVal: (x: number, y: number, val: T | undefined) => void
    i: (x: number, y: number) => number
    row: (y: number) => T[]
    width: number
    height: () => number
    clone(): Grid<T>
    isSame(grid: Grid<any>): boolean
    getData(): (T | undefined)[]
    print(): string
    isEmpty(ee: T): boolean
}

export class DataGrid<T> implements Grid<T> {

    width: number

    constructor(private data: T[], width: number) {
        this.width = width
    }
    at(x: number, y: number) {
        if (x >= this.width || y >= this.height()) return undefined
        return this.data[this.i(x, y)]
    }
    i(x: number, y: number): number {
        return coordsToIndex(x, y, this.width)
    }
    setVal(x: number, y: number, val: T | undefined): boolean | undefined {
        if (val === undefined) return undefined
        this.data[coordsToIndex(x, y, this.width)] = val
        return true
    }
    row(y: number) {
        const offset = y * this.width
        return this.data.slice(offset, offset + this.width)
    }
    height() {
        return Math.floor(this.data.length / this.width)
    }
    clone() {
        return new DataGrid<T>([...this.data], this.width)
    }
    getData() {
        return this.data
    }
    print() {
        return unflat<T>(this.data, this.width).map(item => item.join('')).join('\n')
    }

    isSame(grid: Grid<any>): boolean {
        let _grid = grid.getData()
        let thisData = this.getData()
        let out = true
        thisData.forEach((dd, i) => {
            if (dd != _grid[i]) {
                out = false
            }

        })
        return out

    }

    isEmpty(emptyEntity: T): boolean {
        let out = true
        for (let y = 0; y < this.height(); y++) {
            for (let x = 0; x < this.width; x++) {
                const val = this.at(x, y)
                if (val != emptyEntity) {
                    out = false
                    break
                }
            }
        }
        return out
    }

    static fromGrid(grid: any[][], width?: number) {
        const width_ = width ?? grid[0].length
        return new DataGrid(grid.flat(), width_)
    }
    static createEmpty(width: number, height: number, def: any) {
        const data = Array(width * height).fill(def)
        return new DataGrid(data, width)
    }
}

/*
 * Where g is a grid and queries is a list of things to check for based on getting a hash
 */
export function checkGridForMatches<T>(g: Grid<T>, queries: T[], checkValue: T) {
    const height = g.height()
    for (let y = 1; y < height - 1; y++) {
        for (let x = 1; x < g.width - 1; x++) {
            const m = getMask(getSubArr<T>(x - 1, y - 1, 3, 3, g), checkValue)
        }
    }
}

/*
 * Takes a 2d array and returns sub array of it
 */
export function getSubArr<T>(x: number, y: number, width: number, height: number, arr: Grid<T>): Grid<T> {
    let out: Grid<T> = new DataGrid<T>([], width)
    for (let i = 0; i < height; i++) {
        for (let j = 0; j < width; j++) {
            let found = arr.at(j + x, i + y)
            out.setVal(j, i, found)
        }
    }
    return out
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
    let out: T[][] = []
    const dl = g.length
    const rows = Math.floor(dl / w)
    for (let y = 0; y < rows; y++) {
        out[y] = []
        for (let x = 0; x < w; x++) {
            out[y][x] = g[coordsToIndex(x, y, w)]
        }
    }
    return out
}

/*
 * Takes an array of bits and returns the hex equivalent
 */
export function binaryArrayToHex(arr: number[]): string {
    const binaryString = arr.map(num => num === 0 ? 0 : 1).join('')
    return parseInt(binaryString, 2).toString(16)
}

/*
 * collecting subarrays from one big array, given a width & height you want the subs to be
 */
export function collectSubArr<T>(width: number, height: number, arr: Grid<T>): Grid<T>[] {
    const input_height = arr.height()
    const input_width = arr.width
    let out: Grid<T>[] = []
    if (width > input_width || height > input_height) return out
    for (let i = 0; i < input_height; i += height) {
        for (let j = 0; j < input_width; j += width) {
            out.push(getSubArr<T>(j, i, width, height, arr))
        }
    }
    return out
}

/*
 *
 *
 */
function getMask<T>(arr: Grid<T>, checkVal: T, xOffset: number = 0, yOffset: number = 0): number {
    let out = 0
    out = arr.at(0 + yOffset, 0 + xOffset) == checkVal ? out | Placements.fromTopLeft : out
    out = arr.at(0 + yOffset, 1 + xOffset) == checkVal ? out | Placements.fromTop : out
    out = arr.at(0 + yOffset, 2 + xOffset) == checkVal ? out | Placements.fromTopRight : out
    out = arr.at(1 + yOffset, 0 + xOffset) == checkVal ? out | Placements.fromLeft : out
    out = arr.at(1 + yOffset, 1 + xOffset) == checkVal ? out | Placements.center : out
    out = arr.at(1 + yOffset, 2 + xOffset) == checkVal ? out | Placements.fromRight : out
    out = arr.at(2 + yOffset, 0 + xOffset) == checkVal ? out | Placements.fromBottomLeft : out
    out = arr.at(2 + yOffset, 1 + xOffset) == checkVal ? out | Placements.fromBottom : out
    out = arr.at(2 + yOffset, 2 + xOffset) == checkVal ? out | Placements.fromBottomRight : out
    return out
}

enum Placements {
    fromTopLeft = 0b100000000,
    fromTop = 0b010000000,
    fromTopRight = 0b001000000,
    fromLeft = 0b000100000,
    fromRight = 0b000001000,
    center = 0b000010000,
    fromBottomRight = 0b000000100,
    fromBottom = 0b000000010,
    fromBottomLeft = 0b000000001,
}

export function encodeGrid<T>(arr: Grid<T>, valCheck: T) {
    let out: number = 0b0
    iterateGrid(arr, (x, y, val) => {
        out = val === valCheck ? (out << 1) | 1 : (out << 1) | 0
    })
    return out
}

export function gridFromRegionCode<T>(code: number, g: Grid<T>): Grid<T> {
    const size = g.width * g.height()
    const pad = (Array(size).fill('0')).join('')
    let mask = Number(code).toString(2)
    mask = (pad + mask).slice(-1 * size)
    const maskArr = mask.split('').map(i => parseInt(i))
    const maskGrid = DataGrid.fromGrid(unflat(maskArr, g.width))
    let [minX, minY, maxX, maxY] = [Infinity, Infinity, 0, 0]
    iterateGrid(maskGrid, (x, y, v) => {
        if (v === 1) {
            minX = Math.min(x, minX)
            minY = Math.min(y, minY)
            maxX = Math.max(x, maxX)
            maxY = Math.max(y, maxY)

        }
    })
    let out = getSubArr(minX, minY, (maxX - minX) + 1, (maxY - minY) + 1, g)
    return out
}

/*
 * Grows a Grid n vertically, given a row index to repeat as filler.
 */
export function growGridVertical<T>(n: number, row: number, g: Grid<T>, def: T): Grid<T> {
    let top = getSubArr<T>(0, 0, g.width, row, g)
    const filler = getSubArr<T>(0, row, g.width, 1, g)
    const base = getSubArr<T>(0, row + 1, g.width, g.height() - (row + 1), g)
    if (n > 0) {
        for (let _ in Array(n).fill(0)) {
            top = addChunk(top, filler, 0, top.height(), def)
        }
    }
    top = addChunk(top, base, 0, top.height(), def)
    return top
}

export function findInGrid<T>(patterns: Grid<T> | Grid<T>[], base: Grid<T>): { x: number, y: number }[][] {
    const _patterns = Array.isArray(patterns) ? patterns : [patterns]
    const out = new Array(_patterns.length).fill(0).map(i => [])
    iterateGrid<T>(base, (bx, by, _bv) => {
        for (let pattern in _patterns) {
            let matching = true
            iterateGrid<T>(_patterns[pattern], (px, py, pv) => {
                if (pv != base.at(px + bx, py + by)) matching = false
            })
            if (matching) out[pattern].push({ x: bx, y: by })
        }
    })
    return out
}

export function findAndReplaceAllGrid<T>(patterns: Grid<T> | Grid<T>[],
    replacements: Grid<T> | Grid<T>[],
    base: Grid<T>) {
    const _patterns = Array.isArray(patterns) ? patterns : [patterns]
    const _replacements = Array.isArray(replacements) ? replacements : [replacements]
    const res = findInGrid(_patterns, base)
    for (let pattern in res) {
        for (let item in res[pattern]) {
            base = addChunk(base,
                _replacements[pattern],
                res[pattern][item].x,
                res[pattern][item].y,
                0 as unknown as T) // >:(
        }
    }
    return base
}

export async function readTiledFile(p: string): Promise<TiledRawJSON> {
    const fi = await readFile(p, 'utf-8')
    return JSON.parse(fi)
}




export function createEmptyTiledMap(template: TiledRawJSON, w: number, h: number): TiledRawJSON {
    let out = JSON.parse(JSON.stringify(template)) as TiledRawJSON
    out.layers = out.layers.map(l => {
        l.data = Array(w * h).fill(0)
        l.width = w
        l.height = h
        return l
    })
    out.width = w
    out.height = h
    // out.
    return out
}

export function createLayer(width: number, height: number,
    name: string, id: number): ILayer {
    return {
        data: Array(height * width).fill(0),
        height,
        id,
        name,
        opacity: 1,
        properties: [],
        type: 'tilelayer', // TODO make different
        visible: true,
        width,
        x: 0,
        y: 0,
        draworder: 'topdown',
        objects: []
    }
}


export function getTiledLayerId(map: TiledRawJSON, layerName: string): number | undefined {
    const layer = map.layers.find(l => l.name == layerName)
    if (!layer) return undefined
    return layer.id
}

export function checkTiledLayerProperty(map: TiledRawJSON, li: number, property: string): string | undefined {
    const props = map.layers.find(l => l.id == li).properties
    if (!props) return undefined
    const foundProp = props.find(p => p.name == property)
    if (!foundProp) return undefined
    return foundProp.value
}

export function iterateGrid<T>(arr: Grid<T>, cb: (x: number, y: number, value: T) => void) {
    for (let y = 0; y < arr.height(); y++) {
        for (let x = 0; x < arr.width; x++) {
            const val = arr.at(x, y)
            cb(x, y, val!)
        }
    }
}
function _idOrLayer(layer: string | number, map: TiledMap): number {
    let _layer: number
    if (typeof layer === 'string') {
        const tiledLayer = map.getConf().layers.find(ml => ml.name === layer)
        if (!tiledLayer) throw Error('layer string not found')
        _layer = tiledLayer.id
    } else {
        _layer = layer
    }
    return _layer
}

export function applyTiledReplacements(map: TiledMap, layer: string | number, replacementSet: [Grid[], Grid[]]) {
    let _layer: number = _idOrLayer(layer, map)
    findAndReplaceAllGrid(replacementSet[0], replacementSet[1], map.lg[_layer])
    return map
}

export function getReplacementSet(map: TiledMap, layer: string): [Grid[], Grid[]] {
    const tmap = map.getConf()
    const patternLayer = tmap.layers.find(ml => ml.name === layer + '_patterns')
    const replaceLayer = tmap.layers.find(ml => ml.name === layer + '_replace')
    if (!patternLayer || !replaceLayer) return [[], []]

    const regionString = checkTiledLayerProperty(tmap, patternLayer.id, 'replace_regions')
    if (!regionString) return [[], []]
    const regions = regionString.split(',')
    let out: [Grid[], Grid[]] = [[], []]
    regions.forEach(region => {
        const pRegion = parseInt(region, 16)
        out[0].push(gridFromRegionCode(pRegion, map.lg[patternLayer.id]))
        out[1].push(gridFromRegionCode(pRegion, map.lg[replaceLayer.id]))
    })
    return out
}


export function getMaxXofGrid<T>(g: T[][]): number {
    // for combatability
    return g.map(v => v.length).reduce((p, c) => Math.max(p, c))
    // return g.width
}

export function makeEmptyGrid<T>(w: number, h: number, v: T): Grid<T> {
    return new DataGrid<T>(Array(w * h).fill(v), w)
    // return new Array(h).fill(0).map(_ => new Array(w).fill(v))
}

export function gridCopy<T>(arr: T[][]): T[][] {
    let out: T[][] = []
    arr.forEach((item, i) => out[i] = [...item]);
    return out
}

/*
 * Takes a base grid and puts an overlay (ol) ontop with offset. A default value (def) is given as well.
 *
 * If a negative offset is used, the map will grow to allow it.
 */
export function attachTileChunks<T>(base: Grid<T>, ol: Grid<T>, xo: number, yo: number, def: T): Grid<T> {
    const height = Math.max(base.height(), ol.height() + yo)
    const width = Math.max(base.width, ol.width + xo)
    let out = makeEmptyGrid(width, height, def)
    iterateGrid(out, (x: number, y: number, v: T) => {
        if (base.at(x, y) != undefined) {
            out.setVal(x, y, base.at(x, y)!)
        }
        const olX = x - xo
        const olY = y - yo
        if (olX >= 0 && olY >= 0) {
            out.setVal(x, y, ol.at(olX, olY)!)
        }
    })
    return out
}

/*
 * Quickly injects chunk ol into base and returns it.  Doesn't check for growth,
 * omits ones not landing on base.
 */
export function injectChunk<T>(base: Grid<T>, ol: Grid<T>, xo: number, yo: number): Grid<T> {
    iterateGrid(ol, (x, y, val) => {
        if (base.at(x + xo, y + yo) != undefined) {
            base.setVal(x + xo, y + yo, val)
        }
    })
    return base
}

/*
 * Dynamically pick between inject and attachTileChunk based on needd
 */
export function addChunk<T>(base: Grid<T>, ol: Grid<T>, xo: number, yo: number, def: T): Grid<T> {
    if (xo < 0 || yo < 0) {
        const width = xo < 0 ? Math.max(Math.abs(xo) + base.width, ol.width) :
            Math.max(base.width, xo + ol.width)
        const height = yo < 0 ? Math.max(Math.abs(yo) + base.height(), ol.height()) :
            Math.max(base.height(), yo + ol.height())
        const baseXo = xo < 0 ? Math.abs(xo) : 0
        const baseYo = yo < 0 ? Math.abs(yo) : 0
        const olXo = xo < 0 ? 0 : xo
        const olYo = yo < 0 ? 0 : yo
        let out: Grid<T> = DataGrid.createEmpty(width, height, def)
        out = addChunk(out, base, baseXo, baseYo, def)
        out = addChunk(out, ol, olXo, olYo, def)
        return out
    } else if (base.height() < ol.height() + yo || base.width < ol.width + xo) {
        return attachTileChunks<T>(base, ol, xo, yo, def)
    } else {
        return injectChunk<T>(base, ol, xo, yo)
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
    return g.print()
}
