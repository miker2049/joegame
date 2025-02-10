import { Tilemap } from "@pixi/tilemap";
import { Texture, Container, ColorMatrixFilter, Assets } from "pixi.js";
import { getUnique, makeObjectLayers, string2hex } from "./utils";
import { ObjectTilemap } from "./ObjectTilemap";

// A given wang of 0-15 maps to one of these 4x4 chunks
// the ids here depend on a certain 6x6 map

const WANG_TILES: number[][] = [
    [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
    [0, 14, 19, 20, 0, 0, 13, 26, 0, 0, 0, 14, 0, 0, 0, 0],
    [0, 0, 0, 0, 0, 0, 0, 8, 0, 0, 7, 20, 0, 8, 25, 26],
    [0, 6, 27, 28, 0, 12, 21, 22, 0, 18, 27, 28, 0, 24, 21, 22],
    [0, 0, 0, 0, 9, 0, 0, 0, 25, 9, 0, 0, 19, 20, 5, 0],
    [7, 21, 21, 22, 21, 19, 20, 28, 19, 25, 26, 21, 25, 26, 21, 15],
    [0, 0, 0, 0, 1, 2, 3, 4, 19, 20, 21, 22, 25, 26, 27, 28],
    [7, 26, 27, 28, 19, 20, 21, 22, 25, 26, 27, 28, 19, 20, 21, 22],
    [25, 26, 35, 0, 19, 16, 0, 0, 15, 0, 0, 0, 0, 0, 0, 0],
    [19, 20, 21, 22, 25, 26, 27, 28, 31, 32, 33, 34, 0, 0, 0, 0],
    [19, 20, 19, 9, 19, 20, 25, 26, 19, 20, 21, 21, 14, 26, 27, 28],
    [25, 26, 27, 28, 19, 20, 21, 22, 21, 21, 21, 21, 14, 20, 21, 22],
    [19, 20, 11, 0, 25, 26, 17, 0, 19, 20, 23, 0, 25, 26, 29, 0],
    [20, 21, 22, 19, 26, 27, 28, 25, 20, 21, 22, 19, 26, 27, 28, 16],
    [20, 21, 22, 9, 26, 27, 28, 19, 19, 20, 21, 22, 25, 26, 27, 28],
    [25, 26, 27, 28, 19, 20, 21, 22, 25, 26, 27, 28, 19, 20, 21, 22],
];

type JTilemapConfig = {
    // in pixels
    tileSize: number;
    textureKey: string;
    // width of this map, in tiles
    mapWidth: number;
    // how many tiles across is the tileset, for calculating tile from index
    tilesetWidth: number;
};

export class JTilemap extends Tilemap {
    tileSize: number; // in pixels
    mapWidth: number; // in tiles
    tilesetWidth: number; // in tiles
    textureKey: string;

    constructor({
        tilesetWidth,
        tileSize,
        textureKey,
        mapWidth,
    }: JTilemapConfig) {
        super(Texture.from(textureKey).source);
        this.tileSize = tileSize;
        this.mapWidth = mapWidth;
        this.tilesetWidth = tilesetWidth;
        this.textureKey = textureKey;
    }

    setTile(x: number, y: number, tid: number) {
        if (tid === 0) return;
        const [tx, ty] = this.tidToCoords(tid);
        this.tile(this.textureKey, x * this.tileSize, y * this.tileSize, {
            u: tx * this.tileSize,
            v: ty * this.tileSize,
            tileWidth: this.tileSize,
            tileHeight: this.tileSize,
        });
    }

    setTileGrid(x: number, y: number, g: number[][]) {
        for (let yy = y; yy < g.length; yy++) {
            for (let xx = x; xx < g[0].length; xx++) {
                this.setTile(xx, yy, g[yy][xx]);
            }
        }
    }

    setTileGridArr(x: number, y: number, g: number[], gwidth: number) {
        const rows = g.length / gwidth;
        for (let yy = y; yy < rows + y; yy++) {
            for (let xx = x; xx < gwidth + x; xx++) {
                this.setTile(xx, yy, g[(yy - y) * gwidth + (xx - x)]);
            }
        }
    }

    private tidToCoords(tid: number): [number, number] {
        return [tid % this.tilesetWidth, Math.floor(tid / this.tilesetWidth)];
    }

    renderWangData(wdata: number[], wdataWidth: number) {
        wdata.forEach((it, idx) => {
            const x = idx % wdataWidth;
            const y = Math.floor(idx / wdataWidth);
            this.setTileGridArr(x * 4, y * 4, WANG_TILES[it], 4);
        });
    }

    static createWangLayer({ name, data }: { name: string; data: string }) {
        const tilemap = new JTilemap({
            tileSize: 16,
            textureKey: name,
            mapWidth: 4 * 31,
            tilesetWidth: 6,
        });
        tilemap.renderWangData(string2hex(data), 32);
        return tilemap;
    }

    static async fetchMap(x: number, y: number, file: number, rank: number) {
        const rawdata = await fetch(
            `http://localhost:5000/worldmap/${x}/${y}/${file}/${rank}`,
        );
        const jsondata = await rawdata.json();
        const layers: Tilemap[] = jsondata.wang.map(JTilemap.createWangLayer);
        const objs = new ObjectTilemap(jsondata.objects);
        const container = new Container();

        layers.forEach((it) => container.addChild(it));
        container.addChild(objs);
        return container;
    }
}
