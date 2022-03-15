import TiledRawJSON from '../../src/types/TiledRawJson';
import fs from 'fs/promises'
import { coordsToIndex } from '../../src/utils/indexedCoords'

/*
 * Where g is a grid and queries is a list of things to check for based on getting a hash
 */
export function checkGridForMatches<T>(g: T[][], queries: T[], checkValue: T) {
    for (let y = 1; y < g.length - 1; y++) {
        for (let x = 1; x < g[y].length - 1; x++) {
            const m = getMask(getSubArr<T>(x - 1, y - 1, 3, 3, g), checkValue)
            // console.log(m)
        }
    }
}

/*
 * Takes a 2d array and returns sub array of it
 */
function getSubArr<T>(x: number, y: number, width: number, height: number, arr: T[][]): T[][] {
    let out: T[][] = []
    for (let i = 0; i < height; i++) {
        out[i] = []
        for (let j = 0; j < width; j++) {
            out[i][j] = arr[i + y][j + x]
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
export function collectSubArr<T>(width: number, height: number, arr: T[][]): T[][][] {
    const input_height = arr.length
    const input_width = arr[0].length
    let out: T[][][] = []
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
function getMask<T>(arr: T[][], checkVal: T, xOffset: number = 0, yOffset: number = 0): number {
    let out = 0
    out = arr[0 + yOffset][0 + xOffset] == checkVal ? out | Placements.fromTopLeft : out
    out = arr[0 + yOffset][1 + xOffset] == checkVal ? out | Placements.fromTop : out
    out = arr[0 + yOffset][2 + xOffset] == checkVal ? out | Placements.fromTopRight : out

    out = arr[1 + yOffset][0 + xOffset] == checkVal ? out | Placements.fromLeft : out
    out = arr[1 + yOffset][1 + xOffset] == checkVal ? out | Placements.center : out
    out = arr[1 + yOffset][2 + xOffset] == checkVal ? out | Placements.fromRight : out

    out = arr[2 + yOffset][0 + xOffset] == checkVal ? out | Placements.fromBottomLeft : out
    out = arr[2 + yOffset][1 + xOffset] == checkVal ? out | Placements.fromBottom : out
    out = arr[2 + yOffset][2 + xOffset] == checkVal ? out | Placements.fromBottomRight : out
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

export function encodeGrid<T>(arr: T[][], valCheck: T) {
    let out: number = 0b0
    for (const row of arr) {
        for (const elem of row) {
            out = elem === valCheck ? (out << 1) | 1 : (out << 1) | 0
        }
    }
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

function iterateGrid<T>(arr: T[][], cb: (x: number, y: number, value: T) => void) {
    for (let y = 0; y < arr.length; y++) {
        for (let x = 0; x < arr.length; x++) {
            cb(x, y, arr[y][x])
        }
    }
}

export function getMaxXofGrid<T>(g: T[][]): number {
    return g.map(v => v.length).reduce((p, c) => Math.max(p, c))
}

export function makeEmptyGrid<T>(w: number, h: number, v: T): T[][] {
    // const out: T[][] = []
    // for(let y = 0; y< h; y++){
    //     out[y] = []
    //     for(let x = 0; x< w; x++){
    //         out[y][x] = v
    //     }
    // }
    // return out
    // Weird behavior here..
    // It is because constructing that array in the fill section forces the case that
    // it is the same array in all the spots
    // return new Array(h).fill(new Array(w).fill(v))
    return new Array(h).fill(0).map(_ => new Array(w).fill(v))
}

/*
 * Takes a base grid and puts an overlay (ol) ontop with offset. A default value (def) is given as well.
 */
export function attachTileChunks<T>(base: T[][], ol: T[][], xo: number, yo: number, def: T): T[][] {
    const height = Math.max(base.length, ol.length + yo)
    const width = Math.max(getMaxXofGrid(base), getMaxXofGrid(ol) + xo)
    // // console.log(`This is width ${width}! This is height ${height}!`)
    let out: T[][] = makeEmptyGrid(width, height, def)

    iterateGrid(base, (x: number, y: number, v: T) => {
        if (v == undefined) return
        if (!out[y]) {
            out[y] = []
        }
        out[y][x] = v
    })
    // let out =base
    iterateGrid(ol, (x: number, y: number, v: T) => {
        if (v == undefined) return
        if (!out[y + yo]) {
            out[y + yo] = []
        }
        out[y + yo][x + xo] = v
    })
    return out
}

export function checkForRectangle<T>(g: T[][],
    v: T): [number, number, number, number][] {
    let out: [number, number, number, number][] = []
    iterateGrid(g, (x, y, vo) => {
        if (v == vo) {
            let xc = x
            let yc = y
            //add new corner
            let diagonalRun = true
            while (diagonalRun) {
                if (g[yc + 1][xc + 1] != v) {
                    diagonalRun = false
                    break
                }
                // we will always be a square here, so
                // using the y dimension here is arbitrary
                let missing = false
                for (let i = yc + 1; i >= y; i--) {
                    if (g[i][xc + 1] != v || g[yc + 1][i] != v) {
                        missing = true
                    }
                }
                if (missing) {
                    diagonalRun = false
                } else {
                    xc += 1
                    yc += 1
                }
            }
            // need to find the excess side, it is still a square
            let missingY = false
            let missingX = false
            for (let i = yc; i >= y; i--) {
                if (g[i][xc+1] != v){

                    missingX=true
                } else if ( g[yc + 1][i] != v) {
                    missingY=true
                }
            }
            // Favoring wider to taller squares
            if(!missingX){
                while(!missingX){
                    xc += 1
                    for (let i = yc; i >= y; i--){
                        if(g[i][xc + 1] != v){
                            missingX = true
                        }
                    }

                }
            } else if (!missingY){
                while(!missingY){
                    yc += 1
                    for (let i = xc; i >= x; i--){
                        if(g[yc+1][i] != v){
                            missingY = true
                        }
                    }

                }
            } 
            for(let i = 0; i < yc - y; i++){
                for(let j = 0; j < xc - x; j++){
                    g[y+i][x+i] = undefined as unknown as T
                }
            }
            out.push([x,y,xc,yc])
        }
    })
    return out
}


export function segmentRects() { }
export function printGrid<T>(g: T[][]): string {
    return g.map(v => v.join('') + '\n').join('')
}
