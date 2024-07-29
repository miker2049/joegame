import { Container, Assets, Texture, Sprite, Application } from "pixi.js";

export class TileLayer extends Container {
    t?: Texture;
    grid: Sprite[][];
    globalX: number;
    globalY: number;
    constructor(
        private tileWidth: number,
        private tileHeight: number,
    ) {
        super({
            scale: 1,
            x: 0,
            y: 0,
            width: tileWidth * 256,
            height: tileHeight * 256,
        });
        this.globalX = 0;
        this.globalY = 0;
        this.grid = [
            ...Array(this.tileHeight)
                .fill(0)
                .map((_) => []),
        ];
        this._init().then((_) => console.log("done"));
    }

    async _init() {
        this.t = await Assets.load("/pic.jpg");
        this._initGrid();
    }

    _iterGrid(cb: (xx: number, yy: number) => void) {
        for (let y = 0; y < this.tileHeight; y++)
            for (let x = 0; x < this.tileWidth; x++) cb(x, y);
    }
    _initGrid() {
        this._iterGrid((x, y) => {
            const spr = new Sprite({
                x: x * 256,
                y: y * 256,
                texture: this.t,
            });
            this.grid[y][x] = spr;
            this.addChild(spr);
        });
    }
    _addTile(x: number, y: number) {
        this.addChild(this.grid[y][x]);
    }
}
