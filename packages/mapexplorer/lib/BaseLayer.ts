import { Container } from "pixi.js";
import { Pnt } from "./types";

export class BaseLayer<T> extends Container {
    amount = 2;
    realTileSize = 256;
    grid: T[][] = [];
    iterGrid(cb: (xx: number, yy: number) => void) {
        for (let y = 0; y < this.amount; y++)
            for (let x = 0; x < this.amount; x++) cb(x, y);
    }
    getTile([x, y]: Pnt): Pnt {
        return [
            Math.floor(x / this.realTileSize),
            Math.floor(y / this.realTileSize),
        ];
    }
}
