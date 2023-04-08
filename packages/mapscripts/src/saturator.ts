// -*- lsp-enabled-clients: (deno-ls); -*-
import TiledRawJSON, {
    IObjectLayer,
    PropertyType,
    TiledJsonObject,
    TiledJsonProperty,
} from "../../joegamelib/src/types/TiledRawJson.d.ts";
import { TiledMap } from "./TiledMap.ts";
import {
    DataGrid,
    pathBasename,
    tiledProp,
    TileStacks,
    weightedChoose,
} from "./utils.ts";
import { PackType } from "../../joegamelib/src/types/custom.d.ts";
import { getImage, getCharacter, getObject, MapObjectData } from "./data.ts";
import { jprng } from "./hasher.ts";

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
    firstgid: number,
    name: string
) {
    const tileO = new DataGrid(obj.tile_config.tiles, obj.tile_config.width),
        tileX = Math.floor(obj.x / 16),
        tileY = Math.floor(obj.y / 16);
    const tileFix = new DataGrid(
        tileO.getData().map((it) => it + firstgid),
        tileO.width
    );
    const h = tileFix.height();
    const wo = tileFix.width <= 1 ? 0 : Math.floor(tileFix.width / 2);
    const tm = new TiledMap(tmr);
    tm.addChunkToLayer(name, tileFix, tileX - wo, tileY - h);

    return tm.getConf();
}

export function addObjectTilesToStack(
    obj: {
        tile_config: {
            tiles: number[];
            texture: string;
            width?: number;
        };
        x: number;
        y: number;
    },
    stacks: TileStacks,
    firstgid: number
) {
    const tileO = new DataGrid(obj.tile_config.tiles, obj.tile_config.width),
        tileX = Math.floor(obj.x / 16),
        tileY = Math.floor(obj.y / 16);
    const tileFix = new DataGrid(
        tileO.getData().map((it) => it + firstgid),
        tileO.width
    );
    const h = tileFix.height();
    const wo = tileFix.width <= 2 ? 0 : Math.floor(tileFix.width / 2);

    stacks.addChunk(tileFix, tileX - wo, tileY - h);
}

function pickTileConfig(
    obj: MapObjectData & {
        x: number;
        y: number;
    }
) {
    return {
        ...obj,
        tile_config: {
            texture: obj.tile_config.texture,
            width: 1,
            tiles: [
                weightedChoose(
                    obj.tile_config.tiles,
                    Array(obj.tile_config.tiles.length).fill(
                        1 / obj.tile_config.tiles.length
                    ),
                    jprng(
                        obj.x,
                        obj.y,
                        undefined,
                        "mapobject tileconfig weightedChoose"
                    )
                ),
            ],
        },
    };
}

async function addObjFromTileConfig(
    obj: Pick<MapObjectData, "tile_config"> & { x: number; y: number },
    tm: TiledMap,
    stack: TileStacks
) {
    const imageInfo = await getImage(obj.tile_config.texture);
    if (imageInfo && imageInfo.frameConfig) {
        const gid = tm.addTileset(
            imageInfo.key,
            "../images/" + imageInfo.key + ".png",
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

        addObjectTilesToStack(
            {
                tile_config: obj.tile_config,
                x: obj.x,
                y: obj.y,
            },
            stack,
            gid
        );
    } else console.log(`couldnt add ${obj}`);
}
export async function saturateObjects(m: TiledRawJSON) {
    const tm = new TiledMap(m);
    // cull objects that are ontop of a wall
    tm.cullBlockedObjects("wall");
    const stack = new TileStacks(m.width, m.height);
    // we run through all layers
    const newLayers = await Promise.all(
        m.layers.map(async (lay) => {
            // all object layers need to be dealt with
            if (lay.type === "objectgroup") {
                const olayer = lay as IObjectLayer;
                const newObjs = await Promise.all(
                    olayer.objects.map(async (obj) => {
                        // check whether an object is a mapobject or a character
                        let foundObj = await getObject(obj.type);
                        let foundChar = await getCharacter(obj.name);

                        // check whether this tile_config has been placed
                        const placed = !!(obj.properties || []).find(
                            (p) =>
                                p.name === "tile_config_placed" &&
                                p.value === true
                        );
                        // deal with tile configs
                        if (foundObj && foundObj.tile_config && !placed) {
                            // deal with dynamic "pick" tile_config
                            const merged =
                                foundObj.tile_config.pick === true
                                    ? pickTileConfig({
                                          ...foundObj,
                                          x: obj.x,
                                          y: obj.y,
                                      })
                                    : { ...foundObj, x: obj.x, y: obj.y };
                            // add obj-defined tiles to stack
                            await addObjFromTileConfig(merged, tm, stack);
                            return {
                                ...obj,
                                properties: [
                                    ...(await resolveObjectProps(obj)),
                                    {
                                        type: "bool" as PropertyType,
                                        name: "tile_config_placed",
                                        value: true,
                                    },
                                ],
                            };
                        } else if (obj.type === "convo") {
                            return {
                                ...obj,
                                properties: [
                                    ...(await resolveObjectProps(obj)),
                                ],
                            };
                        } else if (foundChar) {
                            return {
                                ...obj,
                                properties: await resolveObjectProps(obj),
                            };
                        } else return obj;
                    })
                );
                return { ...lay, objects: newObjs };
            } else return lay;
        })
    );
    tm.updateConf({ layers: newLayers });
    applyStackToTiledMap(stack, tm);
    tm.genColliderLayer(
        tm.getGlobalID(23, "browserquestextrude"),
        tm.getGlobalID(26, "browserquestextrude"),
        "COLLIDERS"
    );
    tm.cullLayers();
    return tm.getConf();
}

export function applyStackToTiledMap(stack: TileStacks, tm: TiledMap) {
    const [stacka, stackb] = stack.split((n) => tm.getTileProp(n, "above"));
    if (!stackb.isEmpty())
        tm.applyLgs(stackb.getLgs(), "gen_stack", true, true);
    if (!stacka.isEmpty())
        tm.applyLgs(stacka.getLgs(), "gen_stack_above", true, true);
    return tm;
}

export async function resolveObjectProps<
    T extends {
        name?: string;
        x: number;
        y: number;
        convo?: [string, string][];
        properties?: TiledJsonProperty[];
    }
>(obj: T): Promise<{ name: string; type: PropertyType; value: any }[]> {
    const name = obj.name || "unknown";
    const foundChar = await getCharacter(name);
    const foundObject = await getObject(name);
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
            { name: "scale", type: "int", value: foundChar.scale || 1 },
            { name: "width", type: "int", value: foundChar.width || -1 },
            { name: "height", type: "int", value: foundChar.height || -1 },
            {
                name: "body_off_x",
                type: "int",
                value: foundChar.body?.offsetX || 0,
            },
            {
                name: "body_off_y",
                type: "int",
                value: foundChar.body?.offsetY || 0,
            },
            {
                name: "body_width",
                type: "int",
                value: foundChar.body?.width || -1,
            },
            {
                name: "body_height",
                type: "int",
                value: foundChar.body?.height || -1,
            },
        ];
    } else if (foundObject) {
        return [
            {
                name: "req_image",
                type: "string",
                value: foundObject.req_image.join(","),
            },
            {
                name: "tileobject",
                type: "bool",
                value: foundObject.tile_config ? true : false,
            },
        ];
    } else if (obj.convo) {
        const convoProp = {
            name: "convo",
            type: "string" as PropertyType,
            value: JSON.stringify(obj.convo),
        };
        return [...(obj.properties || []), convoProp];
    } else return [...(obj.properties || [])];
}

/*
 * Expects the tiled map to already be saturated with object props
 */
export async function createPackSection(em: TiledRawJSON) {
    const tm = new TiledMap(em);

    // console.log(tm.allObjects(), "ggy");
    const ts = em.tilesets.map((t) => t.image);
    const objs = tm.allObjects();
    // console.log(objs);
    const fromProps = objs.flatMap((obj) => {
        if (!obj) return [];
        const textures = [
            tiledProp(obj, "texture")?.value,
            ...(tiledProp(obj, "req_image")?.value || "").split(","),
        ];
        // const textures = [];
        return textures.filter((item) => item && item.length > 0);
    });
    const imgs = Array.from(new Set([...ts, ...fromProps]));

    return {
        main: {
            files: await Promise.all(
                imgs.map(async (imp) => {
                    const normalized = pathBasename(imp).replace(/\.png$/, "");
                    const image = await getImage(normalized);

                    if (image && image.frameConfig) {
                        return {
                            type: "spritesheet",
                            key: normalized,
                            url: "/assets/images/" + normalized + ".png",
                            frameConfig: {
                                frameWidth: image.frameConfig.frameWidth || 16,
                                frameHeight:
                                    image.frameConfig.frameHeight || 16,
                                margin: image.frameConfig.margin || 0,
                                spacing: image.frameConfig.spacing || 0,
                            },
                        };
                    } else {
                        return {
                            type: "image",
                            key: normalized,
                            url: "/assets/images/" + normalized + ".png",
                        };
                    }
                })
            ),
        },
        meta: {
            url: "joegame",
        },
    };
}
export function normalizeTilesets(_tm: TiledRawJSON) {}

export function fixTilesetPath(p: string) {
    const ext = p.match(/\.png$/);
    if (!ext) p = p + ".png";
    p = pathBasename(p);
    return;
}

export async function objectsAndPack(
    m: TiledRawJSON
): Promise<TiledRawJSON & { pack: PackType }> {
    await saturateObjects(m);
    return { pack: await createPackSection(m), ...m };
}
