import TiledRawJSON, {
    IObjectLayer,
    TiledJsonProperty,
    TileObjectGroup,
} from "joegamelib/src/types/TiledRawJson";

import type { TiledJsonObject } from "joegamelib/src/types/TiledRawJson";
import {
    DataGrid,
    Grid,
    createEmptyTiledMap,
    createLayer,
    addChunk,
    injectChunk,
    pathBasename,
} from "./utils";
import { coordsToIndex } from "joegamelib/src/utils/indexedCoords";
import { resolveObjectProps } from "./saturator";

export class TiledMap {
    lg: Grid<number>[]; //layer grids
    ready: boolean = false;
    constructor(private config: TiledRawJSON) {
        this.lg = [];
        this.initLgs();
    }

    applyLgs(lgs: Grid<number>[], basename: string, append: boolean = false) {
        const grids = lgs.map((grd, idx) => {
            if (!grd) return;
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

    cullLayers() {
        const filtered = this.config.layers.filter((l) => {
            if (l.type === "objectgroup") {
                return true;
            } else if (l.data) {
                if (l.data.every((v) => v === 0 || v === undefined))
                    return false;
                else return true;
            } else return false;
        });
        this.updateConf({ layers: filtered });
    }

    addChunkToLayer(l: string, grid: Grid<number>, x: number, y: number) {
        let extrasL = this.getLayers().find((d) => d.name === l);
        if (!extrasL) {
            this.addEmptyLayer(l);
            extrasL = this.getLayers().find((d) => d.name === l);
            this.initLgs();
        }

        this.applyLgToLayer(injectChunk(this.lg[extrasL!.id], grid, x, y), l);
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
        const id =
            this.config.layers.reduce(
                (acc, curr) => Math.max(acc, curr.id),
                0
            ) + 1;
        this.config.layers.push({
            type: "objectgroup",
            id,
            x: 0,
            y: 0,
            name,
            opacity: 1,
            properties: [],
            draworder: this.config.layers[0].draworder,
            visible: true,
            objects: [],
        });
        return id;
    }

    addObject(
        type: string,
        x: number,
        y: number,
        layerId: number,
        properties: TiledJsonProperty[] = []
    ) {
        const layer = this.config.layers.find((lay) => lay.id === layerId);
        if (layer) {
            if (layer.type === "objectgroup") {
                const id =
                    layer.objects.reduce(
                        (acc, curr) => Math.max(acc, curr.id),
                        0
                    ) + 1;
                console.log(properties);
                layer.objects.push({
                    type,
                    x,
                    y,
                    id,
                    name: `${type}_${id}`,
                    properties,
                    rotation: 0,
                    visible: true,
                    point: true,
                    height: 0,
                    width: 0,
                });
                return id;
            } else throw Error("Layer for " + type + " is not an object layer");
        } else throw Error("No layer " + layerId + " for  " + type);
    }

    applyObjects(
        objs: { x: number; y: number; type: string }[][],
        prefix: string
    ) {
        const ids = objs.map((_, idx) =>
            this.addObjectLayer(prefix + "_" + idx)
        );
        objs.forEach((group, idx) =>
            group.forEach((obj) =>
                this.addObject(
                    obj.type,
                    obj.x,
                    obj.y,
                    ids[idx],
                    resolveObjectProps(obj)
                )
            )
        );
    }

    /*
     * Get the base tile id of a given id, along with its gid
     */
    getTileID(id: number) {
        const tilesets = [...this.config.tilesets];
        tilesets.sort((a, b) => a.firstgid - b.firstgid);
        while (tilesets.length > 0) {
            const ts = tilesets.pop();
            if (ts) {
                if (id >= ts.firstgid) return [id - ts.firstgid, ts.firstgid];
            }
        }
        return [id, 1];
    }

    getTileProp(thisid: number, prop: string) {
        const [id, gid] = this.getTileID(thisid);
        const tileset = this.config.tilesets.find((tt) => tt.firstgid === gid);
        if (tileset && tileset.tiles) {
            const tile = tileset.tiles.find((tt) => tt.id === id);
            if (tile && tile.properties) {
                const fprop = tile.properties.find((pp) => pp.name === prop);
                if (fprop) return fprop.value;
            }
        }
        return undefined;
    }

    cullBlockedObjects(layername: string) {
        const out = this.config.layers.map((lay) => {
            if (lay.type === "objectgroup") {
                const oobjs: TiledJsonObject[] = [];
                lay.objects.forEach((obj, oidx) => {
                    const tileX = Math.floor(obj.x / this.config.tilewidth);
                    const tileY =
                        Math.floor(obj.y / this.config.tileheight) - 1;
                    const idx = coordsToIndex(tileX, tileY, this.config.width);
                    let ff = false;
                    for (let i = this.config.layers.length - 1; i >= 0; i--) {
                        const tlay = this.config.layers[i];
                        if (tlay.type === "tilelayer") {
                            if (this.getTileProp(tlay.data[idx], layername)) {
                                ff = true;
                                break;
                            }
                        }
                    }
                    if (!ff) oobjs.push(obj);
                });
                return { ...lay, objects: oobjs };
            } else return lay;
        });
        this.updateConf({ layers: out });
    }

    normalizeTilesetPaths() {
        const tilesets = this.config.tilesets.map((ts) => {
            let base = pathBasename(ts.image);
            const ext = base.match(/\.png$/);
            if (!ext) base = base + ".png";
            return { ...ts, image: "../images/" + base };
        });
        this.updateConf({ tilesets });
    }
    hideObjects() {
        let layers = this.config.layers.map((l) => {
            if (l.type === "objectgroup") {
                return { ...l, visible: false };
            } else return l;
        });

        this.updateConf({ layers });
    }

    static createEmpty(height: number, width: number, template: TiledRawJSON) {
        return new TiledMap(createEmptyTiledMap(template, width, height));
    }
}
