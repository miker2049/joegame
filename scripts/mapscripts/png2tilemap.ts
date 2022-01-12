import jimp from 'jimp'
import fs from 'fs/promises'
import TiledRawJSON from '../../src/types/TiledRawJson';
import {coordsToIndex} from '../../src/utils/indexedCoords'


/*
 * https://stackoverflow.com/a/11866980
 */
function toColor(num: number): [number, number, number] {
    num >>>= 0;
    var b = num & 0xFF,
        g = (num & 0xFF00) >>> 8,
        r = (num & 0xFF0000) >>> 16
    return [r, g, b]
}

function concatColorNumbers(r: number, g: number, b: number): number {
    let out = r
    out = (out << 8) | g
    out = (out << 8) | b
    return out
}

function scanImgToGrid(img: jimp) {
    let g: number[][] = []
    for (let i = 0; i < img.bitmap.height; i++) {
        g[i] = []
    }
    img.scan(0, 0, img.bitmap.width, img.bitmap.height, function(x, y, idx) {
        const red = this.bitmap.data[idx + 0];
        const green = this.bitmap.data[idx + 1];
        const blue = this.bitmap.data[idx + 2];
        g[y][x] = concatColorNumbers(red, green, blue)
    });
    return g
}

/*
 * Where g is a grid and queries is a list of things to check for based on getting a hash
 */
function checkGridForMatches<T>(g: T[][], queries: T[], checkValue: T) {
    for (let y = 1; y < g.length - 1; y++) {
        for (let x = 1; x < g[y].length - 1; x++) {
            const m = getMask(getSubArr<T>(x - 1, y - 1, 3, 3, g), checkValue)
            console.log(m)
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
 * takes a grid of presumably pixels, and checks for check vals in 2x2 chunks, in the corners,
 * http://www.cr31.co.uk/stagecast/wang/2corn.html
 * assinging a bitwise number (0-16)
 */
function pixelsToWang2Corners<T>(grid: number[][], check: number): number[][] {
    let out: number[][] = []
    for (let y = 1; y < grid.length - 1; y += 2) {
        out[(y - 1) / 2] = []
        for (let x = 1; x < grid[y].length - 1; x += 2) {
            let n = 0
            grid[y][x] == check ? n |= 0b1000 : undefined
            grid[y][x + 1] == check ? n |= 0b1 : undefined
            grid[y + 1][x] == check ? n |= 0b100 : undefined
            grid[y + 1][x + 1] == check ? n |= 0b10 : undefined
            out[(y - 1) / 2][(x - 1) / 2] = n
        }
    }
    return out
}

/*
 * Takes an array and width, and returns 2d array
 */
function unflat(g: number[], w: number): number[][] {
    let out: number[][] = []
    const dl = g.length
    const rows = Math.floor(dl/w)
    for(let y = 0; y < rows; y++){
        out[y] = []
        for(let x = 0; x < w; x++){
            out[y][x] = g[coordsToIndex(x,y,w)]
        }
    }
    return out
}

/*
 * collecting subarrays from one big array, given a width & height you want the subs to be
 */
function collectSubArr<T>(width: number, height: number, arr: T[][]): T[][][] {
    const input_height = arr.length
    const input_width = arr[0].length
    let out: T[][][] = []
    if (width > input_width || height > input_height) return out
    for (let i = 0; i < Math.floor(input_height / height); i += height) {
        for (let j = 0; j < Math.floor(input_width / width); j += width) {
            out.push(getSubArr<T>(j, i, width, height, arr))
        }
    }
    return out
}

enum Neighborhood {
    TopLeft, Top, TopRight,
    CenterLeft, Center, CenterRight,
    BottomLeft, Bottom, BottomRight
}

/*
 * Read these numbers like this aaabbbccc
 * where it represents a grid/2d arr like
 * aaa
 * bbb
 * ccc
 */

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

export function encodeGrid<T>(arr: T[][], valCheck: T) {
    let out: number = 0b0
    for (const row of arr) {
        for (const elem of row) {
            out = elem === valCheck ? (out << 1) | 1 : (out << 1) | 0
        }
    }
    return out
}



const SpecialSets = {
    empty: [
        [0, 0, 0],
        [0, 0, 0],
        [0, 0, 0]
    ],
    h: [
        [0, 0, 0],
        [1, 1, 1],
        [0, 0, 0]
    ],
    v: [
        [0, 1, 0],
        [0, 1, 0],
        [0, 1, 0]
    ],
    //stop up
    su: [
        [0, 1, 0],
        [0, 1, 0],
        [0, 0, 0]
    ],
    sru: [
        [0, 0, 1],
        [0, 1, 0],
        [0, 0, 0]
    ],
    sr: [
        [0, 0, 0],
        [0, 1, 1],
        [0, 0, 0]
    ],
    srd: [
        [0, 0, 0],
        [0, 1, 0],
        [0, 0, 1]
    ],
    sd: [
        [0, 0, 0],
        [0, 1, 0],
        [0, 1, 0]
    ],
    sld: [
        [0, 0, 0],
        [0, 1, 0],
        [1, 0, 0]
    ],
    s1: [
        [0, 0, 0],
        [1, 1, 0],
        [0, 0, 0]
    ],
    s1u: [
        [1, 0, 0],
        [0, 1, 0],
        [0, 0, 0]
    ],
    //Fork from Horizontal to Vertical, Left Up
    fhvlu: [
        [1, 1, 0],
        [0, 1, 0],
        [0, 1, 0]
    ],
    fhvl: [
        [0, 1, 0],
        [1, 1, 0],
        [0, 1, 0]
    ],
    fhvld: [
        [0, 1, 0],
        [0, 1, 0],
        [1, 1, 0]
    ],
    fhvru: [
        [0, 0, 1],
        [0, 1, 0],
        [0, 0, 0]
    ],
    fhvr: [
        [0, 1, 0],
        [0, 1, 1],
        [0, 1, 0]
    ],
    fhvrl: [
        [0, 1, 0],
        [0, 1, 0],
        [0, 1, 1]
    ],
    //Fork from Vertical to Horizontal, Left
    fvhul: [
        [1, 0, 0],
        [0, 1, 0],
        [1, 1, 1]
    ],
    fvhu: [
        [0, 1, 0],
        [0, 1, 0],
        [1, 1, 1]
    ],
    fvhur: [
        [0, 0, 1],
        [0, 1, 0],
        [1, 1, 1]
    ],
    fvhdl: [
        [1, 1, 1],
        [0, 1, 0],
        [1, 0, 0]
    ],
    fvhd: [
        [1, 1, 1],
        [0, 1, 0],
        [0, 1, 0]
    ],
    fvhdr: [
        [1, 1, 1],
        [0, 1, 0],
        [0, 0, 1]
    ],
    //diagonal right
    dr: [
        [1, 0, 0],
        [0, 1, 0],
        [0, 0, 1]
    ],
    dl: [
        [0, 0, 1],
        [0, 1, 0],
        [1, 0, 0]
    ],

}

const DesertRoads = {
    empty: [
        [0, 0, 0],
        [0, 0, 0],
        [0, 0, 0]
    ],
    h: [
        [0, 0, 0],
        [33, 34, 35],
        [0, 0, 0]
    ],
    v: [
        [0, 24, 0],
        [0, 28, 0],
        [0, 32, 0]
    ],
    //stop up
    su: [
        [0, 1, 0],
        [0, 1, 0],
        [0, 0, 0]
    ],
    sru: [
        [0, 0, 1],
        [0, 1, 0],
        [0, 0, 0]
    ],
    sr: [
        [0, 0, 0],
        [0, 1, 1],
        [0, 0, 0]
    ],
    srd: [
        [0, 0, 0],
        [0, 1, 0],
        [0, 0, 1]
    ],
    sd: [
        [0, 0, 0],
        [0, 1, 0],
        [0, 1, 0]
    ],
    sld: [
        [0, 0, 0],
        [0, 1, 0],
        [1, 0, 0]
    ],
    s1: [
        [0, 0, 0],
        [1, 1, 0],
        [0, 0, 0]
    ],
    s1u: [
        [1, 0, 0],
        [0, 1, 0],
        [0, 0, 0]
    ],
    //Fork from Horizontal to Vertical, Left Up
    fhvlu: [
        [1, 0, 1],
        [0, 1, 1],
        [0, 0, 1]
    ],
    fhvl: [
        [0, 0, 1],
        [1, 1, 1],
        [0, 0, 1]
    ],
    fhvld: [
        [0, 0, 1],
        [0, 1, 1],
        [1, 0, 1]
    ],
    fhvru: [
        [1, 0, 1],
        [1, 1, 0],
        [1, 0, 0]
    ],
    fhvr: [
        [1, 0, 0],
        [1, 1, 1],
        [1, 0, 0]
    ],
    fhvrl: [
        [1, 0, 0],
        [1, 1, 0],
        [1, 0, 1]
    ],
    //Fork from Vertical to Horizontal, Left
    fvhul: [
        [1, 0, 0],
        [0, 1, 0],
        [1, 1, 1]
    ],
    fvhu: [
        [0, 1, 0],
        [0, 1, 0],
        [1, 1, 1]
    ],
    fvhur: [
        [0, 0, 1],
        [0, 1, 0],
        [1, 1, 1]
    ],
    fvhdl: [
        [1, 1, 1],
        [0, 1, 0],
        [1, 0, 0]
    ],
    fvhd: [
        [1, 1, 1],
        [0, 1, 0],
        [0, 1, 0]
    ],
    fvhdr: [
        [1, 1, 1],
        [0, 1, 0],
        [0, 0, 1]
    ],
    //diagonal right
    dr: [
        [1, 0, 0],
        [0, 1, 0],
        [0, 0, 1]
    ],
    dl: [
        [0, 0, 1],
        [0, 1, 0],
        [1, 0, 0]
    ],

}

async function readTiledFile(p: string): Promise<TiledRawJSON> {
    return JSON.parse(await fs.readFile(p, 'utf-8'))
}

function createEmptyTiledMap(template: TiledRawJSON, w: number, h: number): TiledRawJSON {
    template.layers = template.layers.map(l => {
        l.data = Array(w * h).fill(0)
        l.width = w
        l.height = h
        return l
    })
    template.width = w
    template.height = h
    // template.
    return template
}


function getTiledLayerIndex(map: TiledRawJSON, layerName: string): number | undefined {
    const layer=map.layers.findIndex(l=>l.name==layerName)
    if (!layer) return undefined
    return layer
}
function checkTiledLayerColor(map: TiledRawJSON, li: number): number | undefined {
    const props = map.layers[li].properties
    if(!props) return undefined
    const color = props.find(p=>p.name=='color')
    if(!color) return undefined
    return Number('0x'+color.value.substr(3))
}

const WANGSIZE=4

; (async function() {
    let img = await jimp.read("assets/maps/desert/meta-map.png")
    const stamps = await readTiledFile("assets/maps/desert/desert-stamps.json")
    console.log(img.bitmap.width,img.bitmap.height)
    const worldWidth = WANGSIZE * img.bitmap.width
    const worldHeight = WANGSIZE * img.bitmap.height
    const map = createEmptyTiledMap(stamps,worldWidth,worldHeight)

    img = img.resize(img.bitmap.width * 2, img.bitmap.height * 2, jimp.RESIZE_NEAREST_NEIGHBOR)
    const imggrid = scanImgToGrid(img)
    const wangt = pixelsToWang2Corners(imggrid, 0xeec39a)
    console.log(wangt.length, wangt[0].length)
    const stampGrid = unflat(stamps.layers)
    // console.log(wangt[0])

    "hello".substr
    await fs.writeFile('assets/maps/desert/ttmap.json', JSON.stringify(map))
    // const qs = SpecialSets
    // checkGridForMatches<number>(pixels,)
    // img.write('img_test.png')
})()
