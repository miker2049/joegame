
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
 * Takes an array and width, and returns 2d array
 */
export function unflat(g: number[], w: number): number[][] {
    let out: number[][] = []
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
    const binaryString = arr.map(num=> num===0 ? 0 : 1).join('')
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
            // console.log(Math.floor(input_height / height),
            //             height,
            //             width,
            //             input_height,
            //             input_width)
            // console.log(i,j)
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
