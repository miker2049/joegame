// -*- lsp-enabled-clients: (deno-ls); -*-
import { readFileSync } from "node:fs";
import TiledRawJSON from "../../joegamelib/src/types/TiledRawJson.d.ts";
import { pathBasename } from "./utils.ts";
const BASEDIR = "../../assets";
const IMGDIR = BASEDIR + "/images/";

// TODO either make this sync or not
export async function embedTilesetsOffline(
    map: TiledRawJSON
): Promise<TiledRawJSON> {
    let rawmap: TiledRawJSON = Object.assign({}, map);
    for (let i = 0; i < rawmap.tilesets.length; i++) {
        const tileset = rawmap.tilesets[i];
        if (tileset.source) {
            const tilejson = JSON.parse(
                readFileSync(IMGDIR + pathBasename(tileset.source), "utf-8")
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
