import { Tilemap } from "@pixi/tilemap";
import {
    Texture,
    Container,
    Application,
    FederatedPointerEvent,
    PointData,
} from "pixi.js";
import { string2hex, TilemapCache } from "./utils";
import { ObjectTilemap } from "./ObjectTilemap";
import {
    AnimatedSpriteConfig,
    Character,
    createAnimatedSprites,
    Wanderer,
} from "./Character";
import { TILEMAP_TILE_SIZE } from "./constants";
import { MapAddress, WorldMapResponse } from "./types";
import { js as Easystar } from "easystarjs";
import { Viewport } from "pixi-viewport";

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

export class WangLayer extends Tilemap {
    tileSize: number; // in pixels
    mapWidth: number; // in tiles
    tilesetWidth: number; // in tiles
    textureKey: string;
    terrMask: number[][];

    // containsPoint(point: PointData) {
    //     if (Math.random() > 0.5) return true;
    //     else return false;
    // }

    constructor({
        tilesetWidth,
        tileSize,
        textureKey,
        mapWidth,
    }: JTilemapConfig) {
        super(Texture.from(textureKey).source);
        this.tileSize = tileSize;

        this.on("click", (ev) => console.log(ev));
        this.eventMode = "static";
        this.mapWidth = mapWidth;
        this.tilesetWidth = tilesetWidth;
        this.textureKey = textureKey;
        this.terrMask = Array(TILEMAP_TILE_SIZE)
            .fill(null)
            .map((_) => Array(TILEMAP_TILE_SIZE).fill(0));
    }

    handleClick(ev: FederatedPointerEvent) {
        console.log(ev);
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
        if (tid > 0) {
            this.terrMask[y][x] = 1;
        }
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
        const tilemap = new WangLayer({
            tileSize: 16,
            textureKey: name,
            mapWidth: 4 * 32,
            tilesetWidth: 6,
        });
        tilemap.renderWangData(string2hex(data), 32);
        return tilemap;
    }

    randomPosition() {
        return {
            x: Math.random() * this.width,
            y: Math.random() * this.height,
        };
    }
}

export class JTilemap extends Container {
    layers: WangLayer[];
    objects: ObjectTilemap;
    characters: Wanderer[];
    address: MapAddress;
    pathfinder: Easystar;
    constructor(
        [data, addr]: [WorldMapResponse, [number, number, number, number]],
        chars: AnimatedSpriteConfig[],
        app: Application,
    ) {
        super();
        this.eventMode = "static";
        this.address = addr;
        this.layers = data.wang.map(WangLayer.createWangLayer);
        this.objects = new ObjectTilemap(data.objects, this);
        this.pathfinder = new Easystar();
        this.initPathfinder();

        this.characters = chars.map((c) => new Wanderer(c, app, this));
        this.place();
    }

    place() {
        // Map layers
        this.layers.forEach((it) => this.addChild(it));
        this.characters.forEach((spr) => this.addChild(spr));
        this.characters.forEach((spr) => {
            const { x, y } = this.layers[0].randomPosition();
            spr.position.x = x;
            spr.position.y = y;
        });
        this.addChild(this.objects);
    }

    initPathfinder() {
        this.pathfinder.setGrid(this.objects.collisionMap);
        this.pathfinder.setAcceptableTiles([0]);
    }

    onClick({ x, y }: { x: number; y: number }) {
        const tileX = Math.floor(x / this.layers[0].tileSize);
        const tileY = Math.floor(y / this.layers[0].tileSize);
        this.characters[0].active = false;
        this.characters[0].findPath(tileX, tileY).then((path) => {
            this.characters[0].movePath(path);
        });
    }

    static async fetchMap({
        address,
        cache,
        app,
    }: {
        address: MapAddress;
        app: Application;
        cache: TilemapCache;
        viewport: Viewport | undefined;
    }) {
        const res = await cache.getMap(...address);
        const chars: AnimatedSpriteConfig[] = await Promise.all(
            res[0].chars.map(
                (char: string) =>
                    new Promise<AnimatedSpriteConfig>((res, rej) => {
                        createAnimatedSprites(char)
                            .then((c) => res(c))
                            .catch(rej);
                    }),
            ),
        );
        return new JTilemap(res, chars, app);
    }
}
