import { CompositeTilemap } from "@pixi/tilemap";
import { jdb } from "./jdb";
import { getUnique, loadPixelAsset } from "./utils";
import {
    AssetConfig,
    MapObjectConfig,
    ObjectRecord,
    WorldMapResponse,
} from "./types";

export class ObjectTilemap extends CompositeTilemap {
    objectData: Record<string, MapObjectConfig & { assets?: AssetConfig[] }> =
        {};
    collisionMap: number[][];

    constructor(private objs: WorldMapResponse["objects"]) {
        super();
        this.init().then((_) => {
            this.objs.forEach((ob) => this.placeObject(ob));
        });
    }

    async init() {
        const objNames = getUnique(this.objs.map((it) => it[0]));
        await Promise.all(objNames.map((name) => this.loadObject(name)));
        this.objs.sort(this.sortObjs.bind(this));
    }

    sortObjs([nameA, _xA, yA]: ObjectRecord, [nameB, _xB, yB]: ObjectRecord) {
        const objectA = this.objectData[nameA];
        const objectB = this.objectData[nameB];
        if (!objectA || !objectB)
            throw Error("Objectdata not found during sort");
        const heightA = Math.floor(
            objectA.tile_config.tiles.length / objectA.tile_config.width,
        );
        const heightB = Math.floor(
            objectB.tile_config.tiles.length / objectB.tile_config.width,
        );
        const finalA = heightA + yA;
        const finalB = heightB + yB;
        return finalA - finalB;
    }

    placeObject([name, x, y]: [string, number, number]) {
        const objData = this.objectData[name];
        if (!objData || !objData.assets) {
            console.error("bad object name?");
            return;
        }

        const texture = objData.tile_config.texture;
        const asset = objData.assets.find((ass) => ass.key === texture);
        if (!asset) {
            console.error("Can't resolve asset.");
            return;
        }
        const tiles = objData.tile_config.tiles;
        const width = objData.tile_config.width;
        console.log(objData.tile_config.collision);
        const margin = asset.frameConfig?.margin || 0;
        const spacing = asset.frameConfig?.spacing || 0;
        const tileSize = asset.frameConfig?.frameWidth || 16;
        const cols = asset.frameConfig?.columns;
        if (!tiles || !width || !cols) console.error("cant find name");
        tiles.forEach((tile, idx) => {
            const srcTileX = tile % (cols || 20);
            const srcTileY = Math.floor(tile / (cols || 20));
            const srcX = margin + spacing * srcTileX + srcTileX * tileSize;
            const srcY = margin + spacing * srcTileY + srcTileY * tileSize;
            const destTileX = x + (idx % width);
            const destTileY = y + Math.floor(idx / width);
            const destX = destTileX * tileSize;
            const destY = destTileY * tileSize;
            this.tile(name, destX, destY, {
                u: srcX,
                v: srcY,
                tileWidth: tileSize,
                tileHeight: tileSize,
            });
        });
    }

    async loadObject<T extends keyof typeof jdb.mapobjects>(key: T) {
        if (!this.objectData[key]) {
            const obj: (typeof jdb.mapobjects)[T] & { assets?: AssetConfig[] } =
                jdb.mapobjects[key];
            if (!obj) return;
            obj.assets = obj.req_image.map(
                (im: keyof typeof jdb.images) => jdb.images[im],
            );
            await Promise.all(
                obj.assets.map((asset) =>
                    loadPixelAsset(key, `http://localhost:5000/${asset.url}`),
                ),
            );
            this.objectData[key] = obj;
        }
    }
}
