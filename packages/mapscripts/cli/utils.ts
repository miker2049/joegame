// -*- lsp-enabled-clients: (deno-ls); -*-
import TiledRawJSON from "../../joegamelib/src/types/TiledRawJson.d.ts";
import { DB } from "https://deno.land/x/sqlite/mod.ts";
import * as path from "https://deno.land/std@0.97.0/path/mod.ts";
import { saturateObjects, createPackSection } from "../esm/saturator.js";

import { TiledMapCompressed } from "../esm/TiledMapCompressed.js";

const BASEDIR = "/home/mik/joegame/assets";
const IMGDIR = BASEDIR + "/images/";

const db = new DB("jdb.db", { mode: "read" });
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
    const tm = new TiledMapCompressed(outMap);
    tm.cullLayers();
    tm.normalizeTilesetPaths();
    tm.hideObjects();
    tm.compressLayers();
    const final = tm.getConf();
    return { pack: createPackSection(final), ...final };
}

export function getTweetConvo(convo: number) {
    // const db = new DB("jdb.db", { mode: "read" });
    const rows = db.queryEntries<{
        id: number;
        position: number;
        convo_id: number;
        tweet_id: string;
    }>(
        "SELECT DISTINCT position,tweet_id,convo_id FROM tweet_threads WHERE convo_id = ? ORDER BY position",
        [convo]
    );
    const sat = rows.map((r) => ({ ...r, tweet: getTweet(r.tweet_id) }));
    console.log(sat);
    // db.close();
    return sat;
}

function getTweet(id: string) {
    const rows = db.queryEntries<{ id: number; tweet_text: string }>(
        "SELECT * FROM tweets WHERE tweet_id=?",
        [id]
    );
    return rows[0].tweet_text.replaceAll(/@\w+/g, "");
}
