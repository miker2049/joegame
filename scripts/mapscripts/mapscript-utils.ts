import TiledRawJSON, { ILayer } from '../../src/types/TiledRawJson';
import fs from 'fs/promises'
import { coordsToIndex } from '../../src/utils/indexedCoords'


export interface Grid<T> {
    at: (x: number, y: number) => T | undefined
    setVal: (x: number, y: number, val: T | undefined) => void
    i: (x: number, y: number) => number
    row: (y: number) => T[]
    width: number
    height: () => number
    clone(): Grid<T>
    getData(): (T | undefined)[]
    print(): string
}

export class DataGrid<T> implements Grid<T> {

    width: number

    constructor(private data: T[], width: number) {
        this.width = width
    }
    at(x: number, y: number) {
        if(x>=this.width || y>=this.height()) return undefined
        return this.data[this.i(x, y)]
    }
    i(x: number, y: number): number {
        return coordsToIndex(x, y, this.width)
    }
    setVal(x: number, y: number, val: T): void{
       this.data[coordsToIndex(x,y,this.width)] = val
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
    print(){
        return unflat<T>(this.data,this.width).map(item=>item.join('')).join('\n')
    }
    static fromGrid(grid: any[][], width?: number){
        const width_ = width ?? grid[0].length
        return new DataGrid(grid.flat(), width_)
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
            // console.log(m)
        }
    }
}

/*
 * Takes a 2d array and returns sub array of it
 */
export function getSubArr<T>(x: number, y: number, width: number, height: number, arr: Grid<T>): Grid<T> {
    let out: Grid<T> = new DataGrid<T>([],width)
    for (let i = 0; i < height; i++) {
        for (let j = 0; j < width; j++) {
            console.log(i,j)
            const found =arr.at(j + x, i + y)
            if (!found) throw Error('not found')
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

export function gridFromRegionCode<T>(code: number, g: Grid<T>): Grid<T>{
    const size = g.width * g.height()
    const pad = (Array(size).fill('0')).join('')
    let mask = Number(code).toString(2)
    mask = (pad+mask).slice(-1 * size)
    const maskArr = mask.split('')
    const maskGrid = DataGrid.fromGrid(unflat(maskArr, g.width))
    let [minX,minY,maxX,maxY] = [0,0,0,0]
    console.log(maskGrid.print())
    iterateGrid(maskGrid,(x,y,v)=>{
        if(v>0){
            minX = minX === 0 ? x : Math.min(x, minX)
            minY = minY === 0 ? y : Math.min(y, minY)
            maxX = Math.max(x, maxX)
            maxY = Math.max(y, maxY)
        }
    })
    // let out = getSubArr(minX,minY,(maxX-minX) + 1,(maxY-minY)+1,g)
    let out = getSubArr(0,1,2,2,g)
    // iterateGrid(out,(x,y,v)=>{
    //     if(maskGrid.at(x,y)<1){
    //         out.setVal(x,y,undefined)
    //     }
    // })
    return out
}

enum Neighborhood {
    TopLeft, Top, TopRight,
    CenterLeft, Center, CenterRight,
    BottomLeft, Bottom, BottomRight
}

export async function readTiledFile(p: string): Promise<TiledRawJSON> {
    return JSON.parse(await fs.readFile(p, 'utf-8'))
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


export function getTiledLayerIndex(map: TiledRawJSON, layerName: string): number | undefined {
    const layer = map.layers.findIndex(l => l.name == layerName)
    if (!layer) return undefined
    return layer
}

export function checkTiledLayerProperty(map: TiledRawJSON, li: number, property: string): string | undefined {
    const props = map.layers[li].properties
    if (!props) return undefined
    const foundProp = props.find(p => p.type == property)
    if (!foundProp) return undefined
    return foundProp.value
}

function iterateGrid<T>(arr: Grid<T>, cb: (x: number, y: number, value: T) => void) {
    for (let y = 0; y < arr.height(); y++) {
        for (let x = 0; x < arr.width; x++) {
            const val =arr.at(x, y)
            cb(x, y, val!)
        }
    }
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
    if (base.height() < ol.height() + yo || base.width < ol.width + xo) {
        return attachTileChunks<T>(base, ol, xo, yo, def)
    } else {
        return injectChunk<T>(base, ol, xo, yo)
    }
}

export function printGrid<T>(g: Grid<T>): string {
    return g.print()
}
