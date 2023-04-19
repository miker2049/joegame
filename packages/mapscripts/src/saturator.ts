// -*- lsp-enabled-clients: (deno-ls); -*-
import TiledRawJSON, {
    ILayer,
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
import { BasicObject } from "./WorldGenerator.ts";
import { createCrowd, getTweetersFromConvo } from "./tweeters.ts";

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
    const tileO = new DataGrid(
            obj.tile_config.tiles,
            obj.tile_config.width || 1
        ),
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
    if (!obj.tile_config) return obj;
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
    if (!obj.tile_config) return;
    const imageInfo = await getImage(obj.tile_config.texture);
    if (imageInfo && imageInfo.frameConfig) {
        const gid = tm.addTileset(
            imageInfo.key,
            "../images/" + imageInfo.key + ".png",
            {
                margin: imageInfo.frameConfig.margin || 0,
                spacing: imageInfo.frameConfig.spacing || 0,
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
    const newLayers = (await Promise.all(
        m.layers.map(async (lay) => {
            // all object layers need to be dealt with
            if (lay.type === "objectgroup") {
                const olayer = lay as IObjectLayer;
                const newObjs: TiledJsonObject[] = [];
                for await (const sobj of processObjects(
                    olayer.objects,
                    tm,
                    stack
                )) {
                    newObjs.push(sobj as TiledJsonObject);
                }
                return { ...lay, objects: newObjs };
            } else return lay;
        })
    )) as ILayer[];
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

const tweetChars = [
    "tweeter1",
    "tweeter2",
    "tweeter3",
    "tweeter4",
    "tweeter5",
    "tweeter6",
    "tweeter7",
    "tweeter8",
];

async function* processObjects(
    objs: TiledJsonObject[],
    tm: TiledMap,
    stack: TileStacks
): AsyncGenerator<BasicObject> {
    for (const obj of objs) {
        if (obj.type === "mapobject") {
            yield await handleMapObject(obj, tm, stack);
        } else if (obj.type === "convo") {
            // const tw = getTweetersFromConvo(obj.convo);
            const p = obj.properties.find((nn) => nn.name === "convo");
            // console.log(JSON.parse(p.value));
            const tweeters = getTweetersFromConvo(JSON.parse(p.value));
            const places = createCrowd(obj.x, obj.y, tweeters.length, 16);
            for (const tweeter in tweeters) {
                const charName =
                    tweetChars[Math.floor(Math.random() * tweetChars.length)];
                console.log(places);
                const charObj = {
                    name: charName,
                    x: places[tweeter].x,
                    y: places[tweeter].y,
                    type: "char",
                    point: true,
                };
                const out = {
                    ...charObj,
                    properties: await resolveObjectProps(charObj),
                };
                yield { ...out, name: tweeters[tweeter] };
            }

            yield {
                ...obj,
                properties: await resolveObjectProps(obj),
            };
        } else if (obj.type === "char") {
            yield {
                ...obj,
                properties: await resolveObjectProps(obj),
            };
        } else yield obj;
    }
}

async function handleMapObject(
    obj: BasicObject,
    tm: TiledMap,
    stack: TileStacks
): Promise<BasicObject> {
    const foundObj = await getObject(obj.name.split("_")[0]);
    // deal with tile configs
    if (foundObj && foundObj.tile_config) {
        // check whether this tile_config has been placed
        const placed = !!(obj.properties || []).find(
            (p) => p.name === "tile_config_placed" && p.value === true
        );
        if (!placed) {
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
        }
    } else
        return {
            ...obj,
            properties: await resolveObjectProps(obj),
        };
}

export function applyStackToTiledMap(stack: TileStacks, tm: TiledMap) {
    const [stacka, stackb] = stack.split((n) => tm.getTileProp(n, "above"));
    if (!stackb.isEmpty())
        tm.applyLgs(stackb.getLgs(), "gen_stack", true, true);
    if (!stacka.isEmpty())
        tm.applyLgs(stacka.getLgs(), "gen_stack_above", true, true);
    return tm;
}

export async function resolveObjectProps<T extends BasicObject>(
    obj: T
): Promise<{ name: string; type: PropertyType; value: any }[]> {
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
    //
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
