import {
    Container,
    Assets,
    Texture,
    Sprite,
    Application,
    Point,
} from "pixi.js";
import { ObjectPool } from "./utils";

/**
 * LayerView keeps track of tiles in view. A given layer is in a certain scale,
 * which means different tiles are shown based on the LayerViews location. x/y
 * location is always absolute map coordinates, while w/h is tile numbers.
 *
 * 0.5:
 * 000000
 * 0xxxx0
 * 0xXXx0
 * 0xXXx0
 * 0xxxx0
 * 000000
 *
 * 1:
 * xxxx
 * xXXx
 * xXXx
 * xxxx
 *
 * 1.5
 * XX
 * XX
 *
 */
class LayerView {
    x: number;
    y: number;
    // width
    w: number;
    // height
    h: number;
    // scale
    s: number;
    tileSize = 256;
    constructor(x = 0, y = 0, w = 10, h = 10, s = 1) {
        this.x = x;
        this.y = y;
        this.w = w;
        this.h = h;
        this.s = s;
    }

    getGlobalTile(x: number, y: number) {}
}

type TileLayerParameters = {
    screenWidth: number; // in pixels, e.g. "clientWidth"
    screenHeight: number; // in pixels
    gx: number; //absolute, global
    gy: number; //absolute, global
    currZoom: number; //absolute, 0-9
    tileSize: number; // 256
    zoomLevel: number; // this layer's home
};

export class TileLayer extends Container {
    t?: Texture;
    //spritePool: ObjectPool<typeof Sprite>;

    gx: number; // current global offset of x
    gy: number; //                       of y
    tw: number; // width of the main grid in tiles
    th: number; // height of the main grid in tiles
    zoom: number;
    tileSize: number;
    zoomLevel: number; // this layers home zoom level
    grid: Sprite[][];

    constructor({
        screenWidth,
        screenHeight,
        gx,
        gy,
        currZoom,
        tileSize,
        zoomLevel,
    }: TileLayerParameters) {
        super({
            width: tileSize * (4 + Math.ceil(screenWidth / 256)),
            height: tileSize * (4 + Math.ceil(screenHeight / 256)),
            scale: currZoom - (zoomLevel - 1),
            x: gx * ((1 / 256) * Math.pow(2, zoomLevel)) - 2 * tileSize,
            y: gy * ((1 / 256) * Math.pow(2, zoomLevel)) - 2 * tileSize,
        });
        this.tw = 4 + Math.floor(screenWidth / 256);
        this.th = 4 + Math.floor(screenHeight / 256);
        this.gx = gx;
        this.gy = gy;
        this.zoom = currZoom;
        this.tileSize = tileSize;
        this.zoomLevel = zoomLevel;
        //this.spritePool = new ObjectPool((2 + Math.floor(width / 256)) * (2 + Math.floor(height / 256)), Sprite, []);
        this.grid = this.makeSpriteGrid();
        this.placeTiles();
        this._init().then((_) => console.log("done"));
    }

    update(gx: number, gy: number, z: number) {
        if (z < this.zoomLevel - 1 || z > this.zoomLevel + 1) {
            this.iterGrid((x, y) => (this.grid[y][x].visible = false));
        } else {
            this.x =
                gx * ((1 / 256) * Math.pow(2, this.zoomLevel)) -
                2 * this.tileSize;
            this.y =
                gy * ((1 / 256) * Math.pow(2, this.zoomLevel)) -
                2 * this.tileSize;
            this.scale = z - (this.zoomLevel - 1);
        }
    }

    private makeSpriteGrid(): Sprite[][] {
        return Array(this.th)
            .fill(0)
            .map((_) =>
                Array(this.tw)
                    .fill(0)
                    .map((_) => {
                        const spr = new Sprite();
                        this.addChild(spr);
                        return spr;
                    }),
            );
    }

    private async _init() {
        this.t = await Assets.load("/pic.jpg");
        this.iterGrid((x, y) => {
            if (this.t) this.grid[y][x].texture = this.t;
        });
    }

    private iterGrid(cb: (xx: number, yy: number) => void) {
        for (let y = 0; y < this.th; y++)
            for (let x = 0; x < this.tw; x++) cb(x, y);
    }

    private placeTiles() {
        this.iterGrid((x, y) => {
            this.grid[y][x].x = x * this.tileSize;
            this.grid[y][x].y = y * this.tileSize;
        });
    }
}
//
