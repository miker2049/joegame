import { readFileSync } from "node:fs";
import TiledRawJSON, {
    IObjectLayer,
    PropertyType,
} from "joegamelib/src/types/TiledRawJson";
const BASEDIR = "../../assets";
const IMGDIR = BASEDIR + "/images/";
import path from "path";

export async function embedTilesetsOffline(
    map: TiledRawJSON
): Promise<TiledRawJSON> {
    let rawmap: TiledRawJSON = Object.assign({}, map);
    for (let i = 0; i < rawmap.tilesets.length; i++) {
        const tileset = rawmap.tilesets[i];
        if (tileset.source) {
            const tilejson = JSON.parse(
                readFileSync(IMGDIR + path.basename(tileset.source), "utf-8")
            );
            tilejson.image = IMGDIR + tilejson.image;
            rawmap.tilesets[i] = {
                firstgid: rawmap.tilesets[i].firstgid,
                ...tilejson,
            };
        }
    }
    return rawmap;
}
