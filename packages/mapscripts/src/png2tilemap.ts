import jimp from "jimp";
import TiledRawJSON from "joegamelib/src/types/TiledRawJson";
import { coordsToIndex } from "joegamelib/src/utils/indexedCoords";
import {
    addChunk,
    addTilesFromWang,
    checkTiledLayerProperty,
    collectSubArr,
    DataGrid,
    Grid,
    iterateGrid,
    pixelsToWang2Corners,
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
