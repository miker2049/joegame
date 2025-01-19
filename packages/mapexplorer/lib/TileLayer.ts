import { Container, Assets, Texture, Sprite, PointData } from "pixi.js";
import { Pnt } from "./types";
import { TileCache } from "./utils";
import { Tile } from "./Tile";

type TileLayerParameters = {
    screenWidth: number; // in pixels, e.g. "clientWidth"
    screenHeight: number; // in pixels
    tileSize: number; // 256
    zoomLevel: number; // this layer's home
    tcache: TileCache;
};

export class TileLayer extends Container {
    t?: Texture;
    //spritePool: ObjectPool<typeof Sprite>;

    tw: number; // width of the main grid in tiles
    th: number; // height of the main grid in tiles
    rootX: number; // root tile x
    rootY: number; // root tile y
    tileSize: number;
    zoomLevel: number;
    realTileSize: number; // how big this tile is in the world
    grid: Tile[][];
    active: boolean;

    scOffset = 128;
    sc: number; // defines the way the zoom level range is calculated
    tcache: TileCache;

    constructor({
        screenWidth,
        screenHeight,
        tileSize,
        zoomLevel,
        tcache,
    }: TileLayerParameters) {
        super();
        this.tcache = tcache;

        this.x = 0;
        this.y = 0;

        this.tw = Math.ceil(screenWidth / 256);
        this.th = Math.ceil(screenHeight / 256);
        this.tileSize = tileSize;
        this.zoomLevel = zoomLevel;
        // this.realTileSize = (2 ** (8 - zoomLevel)) ** 2;
        // this.scale = 4 / 2 ** zoomLevel;
        this.scale = 2 ** (8 - zoomLevel);
        this.realTileSize = this.scale.x * this.tileSize;
        this.grid = this.makeSpriteGrid();
        this.rootX = 0;
        this.rootY = 0;
        this.active = false;
        this.sc = 2 ** this.zoomLevel / this.scOffset;
        console.log(this.sc, zoomLevel, this.scale);
    }

    // gx,gy is world, z is viewport scale
    update(gx: number, gy: number, z: number) {
        // console.log(z, this.sc, z > this.sc * (3 / 4), z < this.sc * 1.5);
        if (z > this.sc * (3 / 4) && z < this.sc * 1.5) {
            this.active = true;
            this.visible = true;
        } else {
            this.active = false;
            this.visible = true;
        }
        if (!this.active) return;
        const [nrx, nry] = this.getTile([gx, gy]);
        this.rootX = nrx;
        this.rootY = nry;
        this.iterGrid((xx, yy) => this.grid[yy][xx].updateTile(nrx, nry));
    }

    private makeSpriteGrid(): Tile[][] {
        return Array(this.th)
            .fill(0)
            .map((_, yy) =>
                Array(this.tw)
                    .fill(0)
                    .map((_, xx) => {
                        const spr = new Tile({
                            tcache: this.tcache,
                            gridPos: [xx, yy],
                            zoomLevel: this.zoomLevel,
                            tileSize: this.tileSize,
                        });
                        this.addChild(spr);
                        return spr;
                    }),
            );
    }

    private iterGrid(cb: (xx: number, yy: number) => void) {
        for (let y = 0; y < this.th; y++)
            for (let x = 0; x < this.tw; x++) cb(x, y);
    }

    private getTile([x, y]: Pnt): Pnt {
        return [
            Math.floor(x / this.realTileSize),
            Math.floor(y / this.realTileSize),
        ];
    }
}
//
