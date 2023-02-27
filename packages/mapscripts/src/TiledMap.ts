import TiledRawJSON, {
    IObjectLayer,
    TileObjectGroup,
} from "joegamelib/src/types/TiledRawJson";

import type { TiledJsonObject } from "joegamelib/src/types/TiledRawJson";
import {
    DataGrid,
    Grid,
    createEmptyTiledMap,
    createLayer,
    addChunk,
} from "./utils";

export class TiledMap {
    lg: Grid<number>[]; //layer grids
    ready: boolean = false;
    constructor(private config: TiledRawJSON) {
        this.lg = [];
        this.initLgs();
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

    addChunkToLayer(l: string, grid: Grid<number>, x: number, y: number) {
        let extrasL = this.getLayers().find((d) => d.name === l);
        if (!extrasL) {
            this.addEmptyLayer(l);
            extrasL = this.getLayers().find((d) => d.name === l);
            this.initLgs();
        }
        this.applyLgToLayer(addChunk(this.lg[extrasL!.id], grid, x, y, 99), l);
    }

    applyLgToLayer(grid: Grid<number>, layerName: string) {
        let layer = this.getLayers().find((d) => d.name === layerName);
        if (layer && layer.type === "tilelayer") layer.data = grid.getData();
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

    allObjects() {
        return this.config.layers.reduce<TiledJsonObject[]>((acc, curr) => {
            if (curr.type === "objectgroup") {
                return [...acc, ...curr.objects];
            } else return acc;
        }, []);
    }

    addTileset(
        name: string,
        image: string,
        config: {
            margin: number;
            spacing: number;
            tileheight: number;
            tilewidth: number;
            tilecount: number;
            imageheight: number;
            imagewidth: number;
            columns: number;
        }
    ) {
        let exists = this.config.tilesets.find((ts) => ts.name === name);
        if (!exists) {
            const maxTileset = this.config.tilesets.reduce((prev, curr) =>
                prev.firstgid > curr.firstgid ? prev : curr
            );
            const newFirstGid = maxTileset.firstgid + maxTileset.tilecount;

            this.config.tilesets.push({
                name,
                firstgid: newFirstGid,
                image,
                tilewidth: config.tilewidth,
                tileheight: config.tileheight,
                margin: config.margin,
                spacing: config.spacing,
                tilecount: config.tilecount,
                imageheight: config.imageheight,
                imagewidth: config.imagewidth,
                columns: config.columns,
            });
            return newFirstGid;
        } else return exists.firstgid;
    }

    addObjectLayer(name: string) {
        this.config.layers.push({
            type: "objectgroup",
            id:
                this.config.layers.reduce(
                    (acc, curr) => Math.max(acc, curr.id),
                    0
                ) + 1,
            x: 0,
            y: 0,
            name,
            opacity: 1,
            properties: [],
            draworder: this.config.layers[0].draworder,
            visible: true,
            objects: [],
        });
    }

    static createEmpty(height: number, width: number, template: TiledRawJSON) {
        return new TiledMap(createEmptyTiledMap(template, width, height));
    }
}
