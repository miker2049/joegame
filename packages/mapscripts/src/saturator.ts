import { readFileSync } from "fs";
import TiledRawJSON from "joegamelib/src/types/TiledRawJson";
import path from "path";
import { TiledMap } from "./TiledMap";
import { addChunk, DataGrid } from "./utils";

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

export async function addObjectTiles(
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
