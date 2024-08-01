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
    zoom: number;
    tileSize: number;
    zoomLevel: number; // this layers home zoom level
    tileScale: number; // e.g. 1/256,1/128, derived from zoomLevel
    grid: Sprite[][];
    screenWidth: number;
    screenHeight: number;

    private active: boolean;

    constructor({
        screenWidth,
        screenHeight,
        currZoom,
        tileSize,
        zoomLevel,
    }: TileLayerParameters) {
        super({
            scale: 2 ** (currZoom - zoomLevel),
        });
        this.x = screenWidth / 2;
        this.y = screenHeight / 2;
        this.screenWidth = screenWidth;
        this.screenHeight = screenHeight;

        this.tw = Math.ceil(screenWidth / 256);
        this.th = Math.ceil(screenHeight / 256);
        this.tw += this.tw % 2;
        this.th += this.tw % 2;
        this.zoom = currZoom;
        this.tileSize = tileSize;
        this.zoomLevel = zoomLevel;
        this.tileScale = (1 / 256) * 2 ** zoomLevel;
        this.active =
            currZoom > this.zoomLevel - 1 && currZoom < this.zoomLevel + 1;
        //this.spritePool = new ObjectPool((2 + Math.floor(width / 256)) * (2 + Math.floor(height / 256)), Sprite, []);
        this.grid = this.makeSpriteGrid();
        this.placeTiles();
        this._init().then((_) => console.log("done"));
    }

    /**
     * Tile at 0,0.
     */
    private getTile([x, y]: Pnt): Pnt {
        return [Math.floor(x * this.tileScale), Math.floor(y * this.tileScale)];
    }
    private placeContainer([px, py]: Pnt): void {
        const [rootX, rootY] = this.getTile([px, py]);
        const [diffX, diffY] = [px - rootX, py - rootY];
        this.x = -diffX * this.tileScale;
        this.y = -diffY * this.tileScale;
    }
    // private placeContainer(p:) {
    //     const rootTile = [Math.floor()];
    // }

    update(gx: number, gy: number, z: number) {
        if (z < this.zoomLevel - 1 || z > this.zoomLevel + 1) {
            this.active = false;
            this.iterGrid((x, y) => (this.grid[y][x].visible = false));
        } else {
            if (!this.active) {
                this.active = true;
                this.iterGrid((x, y) => (this.grid[y][x].visible = true));
            }
            this.scale = 2 ** (z - this.zoomLevel);
            this.placeContainer([gx, gy]);
            // this.x = gx % (1 / sc);
            // this.y = gy % (1 / sc);
            // console.log(this.zoomLevel, this.x, this.y, gx, sc);
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
