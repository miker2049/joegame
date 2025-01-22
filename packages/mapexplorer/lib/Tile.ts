import {
    Application,
    Graphics,
    GraphicsPath,
    Sprite,
    Text,
    Texture,
} from "pixi.js";
import { TileCache } from "./utils";
import { Pnt } from "./types";
import { TilemapSlots } from "./TilemapSlots";

// askdja akd
export type TileConfig = {
    gridPos: Pnt;
    tcache: TileCache;
    zoomLevel: number;
    tileSize: number;
    app: Application;
};

// askjda
export class Tile extends Sprite {
    currRoot: Pnt = [0, 0];
    loadedTexture?: Pnt;
    tileReady = false;

    gridPos: Pnt;
    tcache: TileCache;
    zoomLevel: number;
    tileSize: number;
    app: Application;
    slots?: TilemapSlots;

    constructor({ gridPos, tcache, zoomLevel, tileSize, app }: TileConfig) {
        super();
        this.app = app;
        this.gridPos = gridPos;
        this.tcache = tcache;
        this.zoomLevel = zoomLevel;
        this.tileSize = tileSize;
        this.roundPixels = true;
        if (zoomLevel === 8) {
            this.slots = new TilemapSlots(this.x, this.y, 32, this);
        }
    }

    private placeTile() {
        const [rx, ry] = this.currRoot;
        const [px, py] = this.gridPos;
        this.x = (rx + px) * this.tileSize;
        this.y = (ry + py) * this.tileSize;
        if (this.slots) {
            this.slots.x = this.x;
            this.slots.y = this.y;
        }

        const g = new Graphics();
        g.rect(this.x, this.y, this.tileSize, this.tileSize);
        g.stroke(0xff0000);
        this.parent.addChild(g);

        const text = new Text({
            text: `${this.zoomLevel}, ${rx + px}, ${ry + py}`,
            x: this.x,
            y: this.y,
        });
        this.parent.addChild(text);
    }

    private loadTexture() {
        const [rx, ry] = this.currRoot;
        const [px, py] = this.gridPos;
        if (
            this.loadedTexture &&
            this.loadedTexture[0] === rx + px &&
            this.loadedTexture[1] === ry + py
        )
            return;
        // console.log(rx + px, ry + py, this.zoomLevel);
        this.tcache
            .getTile(rx + px, ry + py, this.zoomLevel)
            .then(([t, [tx, ty, _]]) => {
                if (
                    tx === this.currRoot[0] + this.gridPos[0] &&
                    ty === this.currRoot[1] + this.gridPos[1]
                ) {
                    this.texture = t;
                    this.loadedTexture = [rx + px, ry + py];
                    this.placeTile();
                }
            });
    }
    updateTile(rx: number, ry: number) {
        this.currRoot = [rx, ry];
        this.loadTexture();
    }
}
