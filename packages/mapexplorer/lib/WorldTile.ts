import { Application, Graphics, Sprite, Text } from "pixi.js";
import { TileCache } from "./utils";
import { DefaultParameters, Pnt } from "./types";
import { config } from "./config";

// askdja akd
export type TileConfig = {
    gridPos: Pnt;
} & DefaultParameters;

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

    constructor(params: TileConfig) {
        super();
        const { gridPos, tcache, zoomLevel, tileSize, app } = params;
        this.app = app;
        this.gridPos = gridPos;
        this.tcache = tcache;
        this.zoomLevel = zoomLevel;
        this.tileSize = tileSize;
        this.roundPixels = true;
    }

    private placeTile() {
        const [rx, ry] = this.currRoot;
        const [px, py] = this.gridPos;
        this.x = (rx + px) * this.tileSize;
        this.y = (ry + py) * this.tileSize;

        if (config.drawWorldTileGrid) {
            const g = new Graphics();
            g.rect(this.x, this.y, this.tileSize, this.tileSize);
            g.stroke(0xff0000);
            this.parent.addChild(g);
        }

        if (config.drawCoordText) {
            const text = new Text({
                text: `${this.zoomLevel}, ${rx + px}, ${ry + py}`,
                x: this.x,
                y: this.y,
            });
            this.parent.addChild(text);
        }
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
    update(rx: number, ry: number) {
        this.currRoot = [rx, ry];
        this.loadTexture();
    }
}
