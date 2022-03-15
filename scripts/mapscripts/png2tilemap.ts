import jimp from 'jimp'
import { unflat, collectSubArr, checkTiledLayerProperty } from './mapscript-utils'
import TiledRawJSON from '../../src/types/TiledRawJson';
import { coordsToIndex } from '../../src/utils/indexedCoords'

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

export function scanImgToGrid(img: jimp) {
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
 * takes a grid of presumably pixels, and checks for check vals in 2x2 chunks, in the corners,
 * http://www.cr31.co.uk/stagecast/wang/2corn.html
 * assinging a bitwise number (0-16)
 */
export function pixelsToWang2Corners<T>(grid: number[][], check: number): number[][] {
    let out: number[][] = []
    for (let y = 1; y < grid.length - 1; y += 2) {
        out[(y - 1) / 2] = []
        for (let x = 1; x < grid[y].length - 1; x += 2) {
            let n = 0
            grid[y][x] == check ? n |= 0b1000 : undefined
            grid[y][x + 1] == check ? n |= 0b1 : undefined
            grid[y + 1][x] == check ? n |= 0b100 : undefined
            grid[y + 1][x + 1] == check ? n |= 0b10 : undefined
            out[(y - 1) / 2][(x - 1) / 2] = n //~n & 15 //only a byte
        }
    }
    return out
}




function checkTiledLayerColor(map: TiledRawJSON, li: number): number | undefined {
    const c = checkTiledLayerProperty(map, li, "color")
    if (!c) return undefined
    return Number('0x' + c.substr(3))
}

function addChunkToLayer(map: TiledRawJSON, li: number, chunk: number[][], originX: number, originY: number) {
    for (let y = 0; y < chunk.length; y++) {
        for (let x = 0; x < chunk[y].length; x++) {
            map.layers[li].data[coordsToIndex(originX + x, originY + y, map.layers[li].width)] = chunk[y][x]
        }
    }
    return map
}



function addTilesFromWang(
    map: TiledRawJSON,
    li: number,
    wangt: number[][],
    stamps: number[][][],
    stampSize: number): TiledRawJSON {

    for (let y = 0; y < wangt.length; y++) {
        for (let x = 0; x < wangt[y].length; x++) {
            const chunk = stamps[wangt[y][x]]
            if (!chunk) console.log('no chunk at index ' + wangt[y][x])
            else map = addChunkToLayer(map, li, chunk, x * stampSize, y * stampSize)
        }
    }

    return map
}

export function applyPixelWangs(stamps: TiledRawJSON, stampSize: number, dest: TiledRawJSON, li: number, img: jimp): TiledRawJSON | undefined {
    const color = checkTiledLayerColor(stamps, li)
    if (!color) {
        console.log("no color..." + stamps.layers[li].name)
        return undefined
    }
    let bigimg = img.clone()
    bigimg.resize(img.bitmap.width * 2, img.bitmap.height * 2, jimp.RESIZE_NEAREST_NEIGHBOR)
    const imggrid = scanImgToGrid(bigimg)
    const wangt = pixelsToWang2Corners(imggrid, color)
    const stampGrid = unflat(stamps.layers[li].data, stamps.layers[li].width)
    const stampsChunks = collectSubArr<number>(stampSize, stampSize, stampGrid)
    dest = addTilesFromWang(dest, li, wangt, stampsChunks, stampSize)
    return dest
}


