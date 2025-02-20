import { Application, Container, Texture } from "pixi.js";
import { DefaultParameters, Pnt, SetCurrentMapFunction } from "./types";
import { TileCache } from "./utils";
import { Tile } from "./WorldTile";
import { Viewport } from "pixi-viewport";

type WorldLayerParameters = {
    screenWidth: number; // in pixels, e.g. "clientWidth"
    screenHeight: number; // in pixels
} & DefaultParameters;

export class WorldLayer extends Container {
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

    // mysterious constant related to zoomLevel
    // Changing this changes how tiles appear or not as we zoom down
    // at 128, and starting at 1/32 zoom, we get 8x3 "3" debug tiles
    // at 64, 3x1 "3" tiles
    scOffset = 178;
    sc: number; // defines the way the zoom level range is calculated

    tcache: TileCache;
    app: Application;

    constructor({
        screenWidth,
        screenHeight,
        tileSize,
        zoomLevel,
        tcache,
        app,
        viewport,
        setCurrentMap,
    }: WorldLayerParameters) {
        super();
        this.app = app;
        this.tcache = tcache;

        this.x = 0;
        this.y = 0;

        this.tw = Math.ceil(screenWidth / 256);
        this.th = Math.ceil(screenHeight / 256);

        // this.tw = 2;
        // this.th = 2;

        this.tileSize = tileSize;
        this.zoomLevel = zoomLevel;
        // Just mapping from its index to some scale
        // Note there are actually *9* layers, but we need that
        // to get this.scale = 1. i.e., 2**0
        this.scale = 2 ** (8 - zoomLevel);
        this.realTileSize = this.scale.x * this.tileSize;
        this.grid = this.makeSpriteGrid(viewport, setCurrentMap);
        this.rootX = 0;
        this.rootY = 0;
        this.active = false;
        this.sc = 2 ** this.zoomLevel / this.scOffset;

        this.interactive = true;
    }

    // gx,gy is world, z is viewport scale
    update(gx: number, gy: number, z: number) {
        // console.log(this.zoomLevel, z, this.sc);

        if (
            (z > this.sc * (3 / 4) && z < this.sc * 1.75) ||
            (z > 2 && this.zoomLevel === 8)
        ) {
            this.active = true;
            this.visible = true;
        } else if (!(z > 4 && this.zoomLevel === 8)) {
            this.active = false;
            this.visible = false;
        }
        if (!this.active) return;
        const [nrx, nry] = this.getTile([gx, gy]);
        this.rootX = nrx;
        this.rootY = nry;
        this.iterGrid((xx, yy) =>
            this.grid[yy][xx].update(nrx, nry, gx, gy, z),
        );
    }

    private makeSpriteGrid(
        viewport: Viewport,
        setCurrentMap: SetCurrentMapFunction,
    ): Tile[][] {
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
                            app: this.app,
                            viewport,
                            setCurrentMap,
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
