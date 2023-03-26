import TiledRawJSON, {
    IObjectLayer,
    PropertyType,
} from "joegamelib/src/types/TiledRawJson";
import { TiledMap } from "./TiledMap";
import {
    DataGrid,
    pathBasename,
    tiledProp,
    TileStacks,
    weightedChoose,
} from "./utils";
import { PackType } from "joegamelib/src/types/custom";
import { getImage, getCharacter, getObject } from "./data";
import { jprng } from "./hasher";

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

export async function saturateObjects(m: TiledRawJSON) {
    const tm = new TiledMap(m);
    tm.cullBlockedObjects("wall");
    const stack = new TileStacks(m.width, m.height);
    for (let layer = 0; layer < m.layers.length; layer++) {
        if (m.layers[layer].type === "objectgroup") {
            const olayer = m.layers[layer] as IObjectLayer;
            await Promise.all(
                olayer.objects.map(async (obj) => {
                    let foundObj = await getObject(obj.type);
                    if (foundObj && foundObj.tile_config) {
                        foundObj = foundObj.tile_config.pick
                            ? {
                                  ...foundObj,
                                  tile_config: {
                                      texture: foundObj.tile_config.texture,
                                      width: 1,
                                      tiles: [
                                          weightedChoose(
                                              foundObj.tile_config.tiles,
                                              Array(
                                                  foundObj.tile_config.tiles
                                                      .length
                                              ).fill(
                                                  1 /
                                                      foundObj.tile_config.tiles
                                                          .length
                                              ),
                                              jprng(obj.x, obj.y)
                                          ),
                                      ],
                                  },
                              }
                            : foundObj;
                        const imageInfo = await getImage(
                            foundObj.tile_config.texture
                        );
                        if (imageInfo && imageInfo.frameConfig) {
                            const gid = tm.addTileset(
                                imageInfo.key,
                                "../images/" + imageInfo.key + ".png",
                                {
                                    margin: imageInfo.frameConfig.margin,
                                    spacing: imageInfo.frameConfig.spacing,
                                    tileheight:
                                        imageInfo.frameConfig.frameHeight,
                                    tilewidth: imageInfo.frameConfig.frameWidth,
                                    imageheight:
                                        imageInfo.frameConfig.imageheight,
                                    imagewidth:
                                        imageInfo.frameConfig.imagewidth,
                                    tilecount: imageInfo.frameConfig.tilecount,
                                    columns: imageInfo.frameConfig.columns,
                                }
                            );

                            addObjectTilesToStack(
                                {
                                    tile_config: foundObj.tile_config,
                                    x: obj.x,
                                    y: obj.y,
                                },
                                stack,
                                gid
                            );
                        }
                    }
                })
            );
        }
    }
    const templg = [...tm.lg];
    const objs = tm.getConf().layers.filter((l) => l.type === "objectgroup");
    const [stacka, stackb] = stack.split((n) => tm.getTileProp(n, "above"));
    tm.applyLgs(stacka.getLgs(), "gen_stack_above");
    tm.applyLgs(stackb.getLgs(), "gen_stack", true);
    tm.applyLgs(templg, "gen", true);
    tm.updateConf({ layers: [...tm.getConf().layers, ...objs] });
    tm.cullLayers();
    return tm.getConf();
}

export async function resolveObjectProps<
    T extends
        | { type: string; name: string }
        | { type: "tweet"; name: string; tweet_text: string }
        | { type: "convo"; name: string; convo: string }
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
    } else if (obj.type === "tweet") {
        return [
            {
                name: "tweet_text",
                type: "string",
                value: obj.tweet_text || "",
            },
        ];
    } else if (obj.type === "convo") {
        return [
            {
                name: "tweet_text",
                type: "string",
                value: JSON.stringify(obj.convo) || "",
            },
        ];
    } else return [];
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
