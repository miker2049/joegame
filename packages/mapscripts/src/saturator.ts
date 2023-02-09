import { readFileSync } from "fs";
import TiledRawJSON, {
    IObjectLayer,
    PropertyType,
} from "joegamelib/src/types/TiledRawJson";
import path from "path";
import { TiledMap } from "./TiledMap";
import { addChunk, DataGrid, tiledProp } from "./utils";
import data from "assets/data.json";

const BASEDIR = "../../assets";
const IMGDIR = BASEDIR + "/images/";

export async function embedTilesetsOffline(
    map: TiledRawJSON
): Promise<TiledRawJSON> {
    let rawmap: TiledRawJSON = Object.assign({}, map);
    rawmap.tilesets.forEach((tileset, i) => {
        if (tileset.source) {
            const tilejson = JSON.parse(
                readFileSync(IMGDIR + path.basename(tileset.source), "utf-8")
            );
            tilejson.image = IMGDIR + tilejson.image;
            rawmap.tilesets[i] = {
                firstgid: rawmap.tilesets[i].firstgid,
                ...tilejson,
            };
        } else {
            return undefined;
        }
    });
    return rawmap;
}

export function addObjectTiles(
    obj: {
        tile_config: {
            tiles: number[];
            texture: string;
            width: number;
        };
        x: number;
        y: number;
    },
    tmr: TiledRawJSON,
    firstgid: number
) {
    const tileO = new DataGrid(obj.tile_config.tiles, obj.tile_config.width),
        tileX = Math.floor(obj.x / 16),
        tileY = Math.floor(obj.y / 16);
    const tileFix = new DataGrid(
        tileO.getData().map((it) => it + firstgid),
        tileO.width
    );
    const center = tileFix.getCenter();
    const tm = new TiledMap(tmr);
    tm.addChunkToLayer("extras", tileFix, tileX - center[0], tileY - center[1]);

    return tm.getConf();
}

export function saturateObjects(m: TiledRawJSON) {
    const tm = new TiledMap(m);
    const objs = tm.allObjects();
    for (let layer = 0; layer < m.layers.length; layer++) {
        if (m.layers[layer].type === "objectgroup") {
            const olayer = m.layers[layer] as IObjectLayer;
            olayer.objects = olayer.objects.map((obj) => {
                const foundObj = getObject(obj.type);
                if (foundObj && foundObj.tile_config) {
                    const imageInfo = getImage(foundObj.tile_config.texture);
                    if (imageInfo && imageInfo.frameConfig) {
                        const gid = tm.addTileset(
                            imageInfo.key,
                            "../images/" + imageInfo.key,
                            {
                                margin: imageInfo.frameConfig.margin,
                                spacing: imageInfo.frameConfig.spacing,
                                tileheight: imageInfo.frameConfig.frameHeight,
                                tilewidth: imageInfo.frameConfig.frameWidth,
                                imageheight: imageInfo.frameConfig.imageheight,
                                imagewidth: imageInfo.frameConfig.imagewidth,
                                tilecount: imageInfo.frameConfig.tilecount,
                                columns: imageInfo.frameConfig.columns,
                            }
                        );
                        addObjectTiles(
                            {
                                ...foundObj,
                                x: obj.x,
                                y: obj.y,
                            },
                            m,
                            gid
                        );
                    }
                }

                return {
                    ...obj,
                    properties: [
                        ...(obj.properties || []),
                        ...resolveObjectProps(obj.type),
                    ],
                };
            });
        }
    }
    return tm.getConf();
}

export function resolveObjectProps(
    name: string
): { name: string; type: PropertyType; value: any }[] {
    const foundChar = getCharacter(name);
    const foundObject = getObject(name);

    if (foundChar) {
        return [
            { name: "texture", type: "string", value: foundChar.texture },
            {
                name: "animNorth",
                type: "string",
                value: foundChar.anims.north.join(","),
            },
            {
                name: "animSouth",
                type: "string",
                value: foundChar.anims.south.join(","),
            },
            {
                name: "animEast",
                type: "string",
                value: foundChar.anims.east.join(","),
            },
            {
                name: "animWest",
                type: "string",
                value: foundChar.anims.west.join(","),
            },
            { name: "speed", type: "int", value: foundChar.speed || 30 },
        ];
    } else if (foundObject) {
        return [
            {
                name: "req_image",
                type: "string",
                value: foundObject.req_image.join(","),
            },
        ];
    } else return [];
}

function getCharacter(name: string) {
    return data.character[name];
}
function getObject(name: string) {
    return data.mapobject[name];
}
function getImage(name: string) {
    return data.image[name];
}

/*
 * Expects the tiled map to already be saturated with object props
 */
export function createPackSection(m: TiledRawJSON) {
    const em = saturateObjects(m);
    const tm = new TiledMap(em);
    const objs = tm.allObjects();
    const imgs = Array.from(
        new Set(
            objs.flatMap((obj) => {
                let imgs = [];
                const textures = [
                    tiledProp(obj, "texture")?.value,
                    ...(tiledProp(obj, "req_image")?.value || "").split(","),
                    ...m.tilesets.map((t) => t.image),
                ];
                return textures.filter((item) => item && item.length > 0);
            })
        )
    );
    return {
        main: {
            files: imgs.map((imp) => {
                const normalized = path.basename(imp).replace(/\.png$/, "");
                const image = getImage(normalized);
                if (image && image.frameConfig) {
                    return {
                        type: "spritesheet",
                        key: normalized,
                        url: "../images/" + normalized + ".png",
                        frameConfig: {
                            frameWidth: image.frameConfig.frameWidth || 16,
                            frameHeight: image.frameConfig.frameHeight || 16,
                            margin: image.frameConfig.margin || 0,
                            spacing: image.frameConfig.spacing || 0,
                        },
                    };
                } else {
                    return {
                        type: "image",
                        key: normalized,
                        url: "../images/" + normalized + ".png",
                    };
                }
            }),
        },
        meta: {
            url: "joegame",
        },
    };
}

export function saturateMap(m: TiledRawJSON) {
    embedTilesetsOffline(m);
    saturateObjects(m);
    return { pack: createPackSection(m), ...m };
}
