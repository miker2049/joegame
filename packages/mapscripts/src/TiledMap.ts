// -*- lsp-enabled-clients: (deno-ls); -*-
import TiledRawJSON, {
    ILayer,
    IObjectLayer,
    ITileLayer,
    ITileLayerInflated,
    TiledJsonProperty,
    TiledJsonObject,
} from "../../joegamelib/src/types/TiledRawJson.d.ts";
import {
    Grid,
    createEmptyTiledMap,
    createLayer,
    addChunk,
    injectChunk,
    pathBasename,
    cullCoordinates,
    mapGrid,
} from "./utils.ts";
import { coordsToIndex } from "../../joegamelib/src/utils/indexedCoords.ts";

export class TiledMap {
    lg: Grid<number>[]; //layer grids
    ready: boolean = false;
    constructor(private config: TiledRawJSON) {
        this.lg = [];
        this.initLgs();
    }

    applyLgs(
        lgs: Grid<number>[],
        basename: string,
        append: boolean = false,
        dontOverwrite = false
    ) {
        const grids = lgs.map((grd, idx) => {
            if (!grd) return;
            const layer = createLayer(
                this.config.width,
                this.config.height,
                basename + "_" + idx,
                idx
            );
            layer.data = grd.getData() as number[];
            return layer;
        }) as ILayer[];
        const newLayers = append ? this.config.layers : [];
        for (let g of grids) {
            // if dontOverwrite is set, and there is already a layer with this name, skip
            if (
                dontOverwrite &&
                !!this.config.layers.find((l: ILayer) => l.name === g.name)
            ) {
                continue;
            } else {
                newLayers.push(g);
            }
        }
        this.config.layers = newLayers.map((l, id) => {
            return { ...l, id };
        });
        this.initLgs();
    }

    initLgs() {
        this.lg = [];
        const tl = this.config.layers.filter((l) =>
            TiledMap.isInflated(l)
        ) as ITileLayerInflated[];
        tl.forEach(
            (layer: ITileLayerInflated) =>
                (this.lg[layer.id] = new Grid(layer.data, layer.width))
        );
    }

    static isTileLayer(l: ILayer): l is ITileLayer {
        return l.type === "tilelayer";
    }
    static isInflated(l: ILayer): l is ITileLayerInflated {
        return l.type === "tilelayer" && typeof l.data !== "string";
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
            } else if (l.data && typeof l.data === "object") {
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
        const layer = this.getLayers().find((d) => d.name === layerName);
        if (layer && TiledMap.isTileLayer(layer))
            layer.data = grid.getData() as number[];
    }

    addEmptyLayer(name: string, visible = true) {
        const layer = createLayer(
            this.config.width,
            this.config.height,
            name,
            this.config.layers.length,
            visible
        );
        this.config.layers.push(layer);
        this.initLgs();
        const id = this.config.layers.find((i) => i.name === name)?.id || 0;
        return id;
    }
    updateDimensionsFromLayer(i: number): void {
        this.updateConf({
            width: this.lg[i].width,
            height: this.lg[i].height(),
        });
        const layer = this.config.layers[i];
        if (TiledMap.isTileLayer(layer)) {
            layer.width = this.lg[i].width;
            layer.height = this.lg[i].height();
        }
    }

    genColliderLayer(wallId: number, walkID: number, name: string) {
        const l = this.addEmptyLayer(name, false);
        this.lg[l] = mapGrid(this.lg[l], (x, y, _) => {
            let blocker = false;
            this.lg.forEach((g, idx) => {
                if (!(idx === l)) {
                    const gv = g.at(x, y);
                    if (gv === undefined)
                        throw new Error(
                            "genColliderLayer: Out of bounds: " + x + ", " + y
                        );
                    if (
                        this.getTileProp(gv, "collides") ||
                        this.getTileProp(gv, "wall")
                    ) {
                        blocker = true;
                    }
                }
            });
            return blocker ? wallId : walkID;
        });
        this.applyLgToLayer(this.lg[l], name);
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
        properties: TiledJsonProperty[] = [],
        name?: string
    ) {
        const layer = this.config.layers.find((lay) => lay.id === layerId);
        if (layer) {
            if (layer.type === "objectgroup") {
                const id =
                    layer.objects.reduce(
                        (acc, curr) => Math.max(acc, curr.id),
                        0
                    ) + 1;
                // console.log(properties);
                layer.objects.push({
                    type,
                    x,
                    y,
                    id,
                    name: `${name || type}_${id}`,
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

    cullObjects() {
        const objs = this.config.layers
            .map((l, idx) => {
                if (l.type === "objectgroup")
                    return l.objects.map((objs) => ({
                        ...objs,
                        priority: idx,
                    }));
                else return [];
            })
            .flatMap((l) => l);
        const out = cullCoordinates(objs, 64);
        this.config.layers.forEach((l, idx) => {
            if (l.type === "objectgroup") {
                (this.config.layers[idx] as IObjectLayer).objects = out.filter(
                    (o) => o.priority === idx
                ) as TiledJsonObject[];
            }
        });
        this.initLgs();
    }

    /*
     * Get the base tile id of a given id, along with its gid
     */
    getTileID(id: number) {
        const tilesets = [...this.config.tilesets];
        let currTileset;
        tilesets.sort((a, b) => a.firstgid - b.firstgid);
        while (tilesets.length > 0) {
            currTileset = tilesets.pop();
            if (currTileset) {
                if (id >= currTileset.firstgid)
                    return [id - currTileset.firstgid, currTileset.firstgid];
            }
        }
        return [id, currTileset?.firstgid];
    }

    /*
     * Get the global id of tile given its tileset id and a tileset name
     */
    getGlobalID(id: number, tsName: string) {
        const tileset = this.config.tilesets.find((t) => t.name === tsName);
        if (!tileset)
            throw Error(
                `${tsName} is not the name of a tileset in a TiledMap getGlobalID func`
            );
        return id + tileset.firstgid;
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
                        if (
                            tlay.type === "tilelayer" &&
                            TiledMap.isInflated(tlay)
                        ) {
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
