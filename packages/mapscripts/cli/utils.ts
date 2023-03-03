// -*- lsp-enabled-clients: (deno-ls); -*-
import TiledRawJSON from "../../joegamelib/src/types/TiledRawJson.d.ts";

import * as path from "https://deno.land/std@0.97.0/path/mod.ts";
import { saturateObjects, createPackSection } from "../esm/saturator.js";
const BASEDIR = "/home/mik/joegame/assets";
const IMGDIR = BASEDIR + "/images/";

export function embedTilesetsOffline(map: TiledRawJSON): TiledRawJSON {
    const rawmap: TiledRawJSON = Object.assign({}, map);
    for (let i = 0; i < rawmap.tilesets.length; i++) {
        const tileset = rawmap.tilesets[i];
        if (tileset.source) {
            const tilejson = JSON.parse(
                Deno.readTextFileSync(IMGDIR + path.basename(tileset.source))
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

export function finalizeTiledmap(map: TiledRawJSON) {
    let outMap = embedTilesetsOffline(map);
    outMap = saturateObjects(outMap);
    return { pack: createPackSection(outMap), ...outMap };
}
