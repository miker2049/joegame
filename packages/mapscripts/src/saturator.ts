import TiledRawJSON, {
    IObjectLayer,
    PropertyType,
} from "joegamelib/src/types/TiledRawJson";
import { TiledMap } from "./TiledMap";
import {
    addChunk,
    DataGrid,
    pathBasename,
    tiledProp,
    TileStacks,
} from "./utils";
import { PackType } from "joegamelib/src/types/custom";
import { getImage, getCharacter, getObject } from "./data";

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
            width: number;
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
    const wo = tileFix.width <= 1 ? 0 : Math.floor(tileFix.width / 2);

    stacks.addChunk(tileFix, tileX - wo, tileY - h);
}

export function saturateObjects(m: TiledRawJSON) {
    const tm = new TiledMap(m);
    const stack = new TileStacks(m.width, m.height);
    for (let layer = 0; layer < m.layers.length; layer++) {
        if (m.layers[layer].type === "objectgroup") {
            const olayer = m.layers[layer] as IObjectLayer;
            olayer.objects.forEach((obj) => {
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
                        addObjectTilesToStack(
                            {
                                ...foundObj,
                                x: obj.x,
                                y: obj.y,
                            },
                            stack,
                            gid
                        );
                    }
                }
            });
        }
    }
    const templg = [...tm.lg];
    const [stacka, stackb] = stack.split((n) => tm.getTileProp(n, "above"));
    tm.applyLgs(stacka.getLgs(), "gen_stack_above");
    tm.applyLgs(stackb.getLgs(), "gen_stack", true);
    tm.applyLgs(templg, "gen", true);
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
            {
                name: "tileobject",
                type: "bool",
                value: foundObject.tile_config ? true : false,
            },
        ];
    } else return [];
}

/*
 * Expects the tiled map to already be saturated with object props
 */
export function createPackSection(em: TiledRawJSON) {
    const tm = new TiledMap(em);
    const ts = em.tilesets.map((t) => t.image);
    const objs = tm.allObjects();
    const fromProps = objs.flatMap((obj) => {
        const textures = [
            tiledProp(obj, "texture")?.value,
            ...(tiledProp(obj, "req_image")?.value || "").split(","),
        ];
        return textures.filter((item) => item && item.length > 0);
    });
    const imgs = Array.from(new Set([...ts, ...fromProps]));

    return {
        main: {
            files: imgs.map((imp) => {
                const normalized = pathBasename(imp).replace(/\.png$/, "");
                const image = getImage(normalized);

                if (image && image.frameConfig) {
                    return {
                        type: "spritesheet",
                        key: normalized,
                        url: "/assets/images/" + normalized + ".png",
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
                        url: "/assets/images/" + normalized + ".png",
                    };
                }
            }),
        },
        meta: {
            url: "joegame",
        },
    };
}

export function objectsAndPack(
    m: TiledRawJSON
): TiledRawJSON & { pack: PackType } {
    saturateObjects(m);
    return { pack: createPackSection(m), ...m };
}
