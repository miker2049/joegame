import { Container, Assets, Texture, Sprite, PointData } from "pixi.js";
import { Pnt } from "./types";

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

    tw: number; // width of the main grid in tiles
    th: number; // height of the main grid in tiles
    rootX: number; // root tile x
    rootY: number; // root tile y
    tileSize: number;
    realTileSize: number; // how big this tile is in the world
    grid: Sprite[][];

    private active: boolean;

    constructor({
        screenWidth,
        screenHeight,
        tileSize,
        zoomLevel,
    }: TileLayerParameters) {
        super();

        this.x = 0;
        this.y = 0;

        this.tw = Math.ceil(screenWidth / 256);
        this.th = Math.ceil(screenHeight / 256);
        this.tw += this.tw % 2;
        this.th += this.tw % 2;
        this.tileSize = tileSize;
        // this.realTileSize = (2 ** (8 - zoomLevel)) ** 2;
        this.scale = 4 / 2 ** zoomLevel;
        this.realTileSize = this.scale.x * this.tileSize;
        this.grid = this.makeSpriteGrid();
        this.rootX = 0;
        this.rootY = 0;
        this.placeTiles();
        this._init().then((_) => console.log("done"));
    }

    update(gx: number, gy: number, z: number) {
        const [nrx, nry] = this.getTile([gx, gy]);
        if (this.rootX !== nrx || this.rootY !== nry) {
            this.rootX = nrx;
            this.rootY = nry;
            this.placeTiles();
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
            this.grid[y][x].x = (x + this.rootX) * this.tileSize;
            this.grid[y][x].y = (y + this.rootY) * this.tileSize;
        });
    }

    private getTile([x, y]: Pnt): Pnt {
        return [
            Math.floor(x / this.realTileSize),
            Math.floor(y / this.realTileSize),
        ];
    }
}
//
