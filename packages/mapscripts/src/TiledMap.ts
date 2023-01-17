import { Jimp } from "@jimp/core";
import path from "path";
import { writeFile, readFile } from "fs/promises";
import jimp from "jimp";
import TiledRawJSON from "joegamelib/src/types/TiledRawJson";
import { DataGrid, Grid, createEmptyTiledMap, createLayer } from "./utils";
import { readTiledFile } from "./utils-node";

export class TiledMap {
    lg: Grid<number>[]; //layer grids
    tilesetImages: Jimp[];
    ready: boolean = false;
    constructor(private config: TiledRawJSON) {
        this.lg = [];
        this.initLgs();
        this.loadTilesetImages()
            .then((_) => (this.ready = true))
            .catch((err) => console.error(err));
    }

    applyLgs(lgs: Grid<number>[], basename: string, append: boolean = false) {
        const grids = lgs.map((grd, idx) => {
            const layer = createLayer(
                this.config.width,
                this.config.height,
                basename + "_" + idx,
                idx
            );
            layer.data = grd.getData();
            return layer;
        });
        this.config.layers = grids.concat(append ? this.config.layers : []);
        this.config.layers = this.config.layers.map((l, id) => {
            return { id, ...l };
        });
        this.initLgs();
    }
    initLgs() {
        this.lg = [];
        this.config.layers.forEach(
            (layer) =>
                (this.lg[layer.id] = new DataGrid(layer.data, layer.width))
        );
    }

    getConf(): TiledRawJSON {
        return this.config;
    }

    updateConf(input: Partial<TiledRawJSON>) {
        this.config = Object.assign(this.config, input);
        this.initLgs();
    }

    getLayers() {
        return this.config.layers;
    }

    addEmptyLayer(name: string) {
        const layer = createLayer(
            this.config.width,
            this.config.height,
            name,
            this.config.layers.length
        );
        this.config.layers.push(layer);
        this.initLgs();
        const id = this.config.layers.find((i) => i.name === name).id;
        return id;
    }
    updateDimensionsFromLayer(i: number): void {
        this.updateConf({
            width: this.lg[i].width,
            height: this.lg[i].height(),
        });
        this.config.layers[i].width = this.lg[i].width;
        this.config.layers[i].height = this.lg[i].height();
    }

    async loadTilesetImages() {
        this.tilesetImages = await Promise.all(
            this.config.tilesets.map(async (tiles) => {
                let file = "";
                if (tiles["source"]) {
                    const tilesetFile = await readFile(
                        "assets/tilesets/" + path.basename(tiles.source),
                        "utf-8"
                    );
                    // HACK hard path
                    file =
                        "assets/tilesets/" +
                        path.basename(JSON.parse(tilesetFile).image);
                } else if (tiles["image"]) {
                    file = "assets/tilesets/" + path.basename(tiles.image);
                } else {
                    return undefined;
                }
                return jimp.read(file);
            })
        );
    }

    static createEmpty(height: number, width: number, template: TiledRawJSON) {
        return new TiledMap(createEmptyTiledMap(template, width, height));
    }
}

export async function tiledMapFromFile(filename: string) {
    const file = await readTiledFile(filename);
    return new TiledMap(file);
}
