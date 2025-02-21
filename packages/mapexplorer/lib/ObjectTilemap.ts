import { CompositeTilemap } from "@pixi/tilemap";
import { jdb } from "./jdb";
import { getUnique, loadPixelAsset } from "./utils";
import {
    AssetConfig,
    MapObjectConfig,
    ObjectRecord,
    WorldMapResponse,
} from "./types";
import { TILEMAP_TILE_SIZE } from "./constants";
import { Graphics } from "pixi.js";
import { config } from "./config";
import { JTilemap } from "./JTilemap";

export class ObjectTilemap extends CompositeTilemap {
    objectData: Record<string, MapObjectConfig & { assets?: AssetConfig[] }> =
        {};
    collisionMap: number[][];
    draw?: Graphics;
    drawerAdded = false;

    constructor(
        private objs: WorldMapResponse["objects"],
        private parentJTilemap: JTilemap,
    ) {
        super();
        this.collisionMap = Array(TILEMAP_TILE_SIZE)
            .fill(null)
            .map((_) => Array(TILEMAP_TILE_SIZE).fill(0));
        this.init()
            .then((_) => {
                this.objs.forEach((ob) => {
                    this.setObjCollisions(ob);
                    this.placeObject(ob);
                });
            })
            .catch((err) => {
                console.error("Couldn't init objects");
                console.error(err);
            });

        if (config.drawCollisionTiles) {
            this.draw = new Graphics();
        }
    }

    async init() {
        const objNames = getUnique(this.objs.map((it) => it[0]));
        await Promise.all(objNames.map((name) => this.loadObject(name)));
        this.objs.sort(this.sortObjs.bind(this));
    }

    private sortObjs(
        [nameA, _xA, yA]: ObjectRecord,
        [nameB, _xB, yB]: ObjectRecord,
    ) {
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

    private setObjCollisions([name, x, y]: ObjectRecord) {
        const object = this.objectData[name];
        if (!object) throw Error("No object data: " + name);
        const { width, collision } = object.tile_config;
        if (collision) {
            collision.forEach((tile, idx) => {
                if (tile === 1) {
                    const cx = (idx % width) + x;
                    const cy = Math.floor(idx / width) + y;
                    if (
                        cx >= this.collisionMap[0].length ||
                        cy >= this.collisionMap.length
                    )
                        return;
                    else {
                        this.collisionMap[cy][cx] = 1;
                    }
                }
            });
        }
    }
    private placeObject([name, x, y]: [string, number, number]) {
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

            if (
                config.drawCollisionTiles &&
                this.draw &&
                this.collisionMap[destTileY][destTileX] === 1
            ) {
                this.draw.rect(destX, destY, tileSize, tileSize);
                this.draw.stroke(0xff0000);
                this.draw.fill({
                    color: 0x110000,
                    alpha: 0.4,
                });
                if (!this.drawerAdded) {
                    this.parentJTilemap.addChild(this.draw);
                }
            }
        });
    }

    private async loadObject<T extends keyof typeof jdb.mapobjects>(key: T) {
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
