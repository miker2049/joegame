import jimp from "jimp";
import TiledRawJSON from "joegamelib/src/types/TiledRawJson";
import { coordsToIndex } from "joegamelib/src/utils/indexedCoords";
import {
    addChunk,
    checkTiledLayerProperty,
    collectSubArr,
    DataGrid,
    Grid,
    iterateGrid,
    unflat,
} from "./utils";
import { TiledMap } from "./TiledMap";

/*
 * https://stackoverflow.com/a/11866980
 */
function toColor(num: number): [number, number, number] {
    num >>>= 0;
    var b = num & 0xff,
        g = (num & 0xff00) >>> 8,
        r = (num & 0xff0000) >>> 16;
    return [r, g, b];
}

function concatColorNumbers(r: number, g: number, b: number): number {
    let out = r;
    out = (out << 8) | g;
    out = (out << 8) | b;
    return out;
}

export function scanRGBToGrid(img: jimp) {
    let g = DataGrid.createEmpty(img.bitmap.width, img.bitmap.height, 0);
    img.scan(0, 0, img.bitmap.width, img.bitmap.height, function (x, y, idx) {
        const red = this.bitmap.data[idx + 0];
        const green = this.bitmap.data[idx + 1];
        const blue = this.bitmap.data[idx + 2];
        g.setVal(x, y, concatColorNumbers(red, green, blue));
    });
    return g;
}

export function scanAlphaToGrid(img: jimp) {
    let g = DataGrid.createEmpty(img.bitmap.width, img.bitmap.height, 0);
    img.scan(0, 0, img.bitmap.width, img.bitmap.height, function (x, y, idx) {
        const alpha = this.bitmap.data[idx + 3];
        g.setVal(x, y, alpha);
    });
    return g;
}

/*
 * takes a grid of presumably pixels, and checks for check vals in 2x2 chunks, in the corners,
 * http://www.cr31.co.uk/stagecast/wang/2corn.html
 * assinging a bitwise number (0-16)
 */
export function pixelsToWang2Corners(
    grid: Grid<number>,
    check: number
): Grid<number> {
    const gheight = grid.height();
    const out = DataGrid.createEmpty(grid.width / 2, gheight / 2, 0);
    for (let y = 1; y < grid.width - 1; y += 2) {
        for (let x = 1; x < gheight - 1; x += 2) {
            let n = 0;
            grid.at(x, y) == check ? (n |= 0b1000) : undefined;
            grid.at(x + 1, y) == check ? (n |= 0b1) : undefined;
            grid.at(x, y + 1) == check ? (n |= 0b100) : undefined;
            grid.at(x + 1, y + 1) == check ? (n |= 0b10) : undefined;

            out.setVal((x - 1) / 2, (y - 1) / 2, n);
        }
    }
    return out;
}

function checkTiledLayerColor(
    map: TiledRawJSON,
    li: number
): number | undefined {
    const c = checkTiledLayerProperty(map, li, "color");
    if (!c) return undefined;
    return Number("0x" + c.substr(3));
}

function addChunkToLayer(
    map: TiledRawJSON,
    li: number,
    chunk: number[][],
    originX: number,
    originY: number
) {
    for (let y = 0; y < chunk.length; y++) {
        for (let x = 0; x < chunk[y].length; x++) {
            map.layers[li].data[
                coordsToIndex(originX + x, originY + y, map.layers[li].width)
            ] = chunk[y][x];
        }
    }
    return map;
}

function addTilesFromWang(
    wangt: Grid<number>,
    stamps: Grid<number>[],
    stampSize: number
): Grid<number> {
    const out = DataGrid.createEmpty(
        wangt.width * stampSize,
        wangt.height() * stampSize,
        0
    );
    iterateGrid(wangt, (x, y, val) => {
        const chunk = stamps[val];
        if (!chunk) console.log("no chunk at index " + wangt.at(x, y));
        else addChunk(out, chunk, x * stampSize, y * stampSize, 0);
    });
    return out;
}

export function applyPixelWangs(
    stampGrid: Grid<number>,
    stampSize: number,
    color: number,
    img: jimp
): Grid<number> | undefined {
    let bigimg = img.clone();
    bigimg.resize(
        img.bitmap.width * 2,
        img.bitmap.height * 2,
        jimp.RESIZE_NEAREST_NEIGHBOR
    );
    const imggrid = scanRGBToGrid(bigimg);
    const wangt = pixelsToWang2Corners(imggrid, color);
    const stampsChunks = collectSubArr<number>(stampSize, stampSize, stampGrid);
    const out = addTilesFromWang(wangt, stampsChunks, stampSize);
    if (!out) console.log(out);
    return out;
}

export function getWangColorGrids(mapp: TiledMap): [number, Grid<number>][] {
    const out: [number, Grid][] = [];
    mapp.getLayers().forEach((l, idx) => {
        if (l.properties) {
            // console.log(l.properties)
            const foundProp = l.properties.find((p) => p.type == "color");
            if (foundProp && foundProp.value) {
                out.push([
                    Number("0x" + foundProp.value.substr(3)),
                    new DataGrid(l.data, l.width),
                ]);
            }
        }
    });
    return out;
}
