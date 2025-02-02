import { Viewport } from "pixi-viewport";
import { Container } from "pixi.js";
import type { MapAddress, Pnt } from "./types";
import { JTilemap } from "./JTilemap";
import { BaseLayer } from "./BaseLayer";
import _, { debounce } from "underscore";

export class TilemapLayer extends BaseLayer<TilemapTile> {
    // the size of the grid of tilemaps
    amount = 1;
    active = false;
    realTileSize = 32;
    rootX = 0;
    rootY = 0;

    constructor(private viewport: Viewport) {
        super();
        this.grid = this.makeSpriteGrid();
    }
    private makeSpriteGrid(): TilemapTile[][] {
        return Array(this.amount)
            .fill(0)
            .map((_, yy) =>
                Array(this.amount)
                    .fill(0)
                    .map((_, xx) => {
                        const spr = new TilemapTile([xx, yy], this.viewport);
                        this.addChild(spr);
                        return spr;
                    }),
            );
    }
    update(gx: number, gy: number, z: number) {
        // console.log(this.zoomLevel, z, this.sc);

        if (z > 32) {
            this.active = true;
            this.visible = true;
        } else {
            this.active = false;
            this.visible = false;
        }
        if (!this.active) return;
        const [nrx, nry] = this.getTile([gx, gy]);
        this.rootX = nrx;
        this.rootY = nry;
        this.iterGrid((xx, yy) => this.grid[yy][xx].update(nrx, nry));
    }
}

export class TilemapTile extends Container {
    gridPos: Pnt;
    currRoot: Pnt;
    tileSize = 32;
    current: MapAddress;
    // how many file/ranks in a tile
    pertile = 8;
    loading = false;

    constructor(
        gridPos: Pnt,
        private viewport: Viewport,
    ) {
        super();
        this.gridPos = gridPos;
        this.currRoot = [0, 0];
        this.current = [0, 0, 0, 0];
    }

    private placeMap() {
        const [rx, ry] = this.currRoot;
        const [px, py] = this.gridPos;
        this.x = (rx + 0) * this.tileSize;
        this.y = (ry + 0) * this.tileSize;
    }

    private load() {
        if (this.loading) return;
        const address = getAddress(
            this.currRoot[0],
            this.currRoot[1],
            this.pertile,
        );
        if (!this.eqlsAddress(address)) {
            this.loading = true;
            this.current = address;
            this.removeChildren();
            try {
                JTilemap.fetchMap(...address).then((tm) => {
                    tm.scale = 1 / 64;
                    this.addChild(tm);
                    this.placeMap();
                });
            } finally {
                this.loading = false;
            }
        }
    }
    update(rx: number, ry: number) {
        this.currRoot = [rx, ry];
        debounce(this.load, 1000);
        this.load();
    }
    getCurrAddress(): MapAddress {
        return getAddress(this.currRoot[0], this.currRoot[1], this.pertile);
    }

    eqlsAddress(other: MapAddress) {
        const curr = this.current;
        return (
            curr[0] === other[0] &&
            curr[1] === other[1] &&
            curr[2] === other[2] &&
            curr[3] === other[3]
        );
    }
}

/**
 * Takes a normalized root, (per 32 unit tile)
 */
function getAddress(x: number, y: number, pertile = 8): MapAddress {
    const tx = Math.floor(x / pertile);
    const ty = Math.floor(y / pertile);
    const file = x % pertile;
    const rank = y % pertile;
    return [tx, ty, file, rank];
}
