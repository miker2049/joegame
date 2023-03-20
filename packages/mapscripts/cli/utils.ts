// -*- lsp-enabled-clients: (deno-ls); -*-
import TiledRawJSON from "../../joegamelib/src/types/TiledRawJson.d.ts";
import { DB } from "https://deno.land/x/sqlite/mod.ts";
import * as path from "https://deno.land/std@0.97.0/path/mod.ts";
import { saturateObjects, createPackSection } from "../esm/saturator.js";

import { TiledMapCompressed } from "../esm/TiledMapCompressed.js";
import { TiledMap } from "../esm/TiledMap.js";

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

export async function finalizeTiledmap(map: TiledRawJSON) {
    let outMap = embedTilesetsOffline(map);
    outMap = await saturateObjects(outMap);
    const tm = new TiledMap(outMap);
    tm.cullLayers();
    tm.normalizeTilesetPaths();
    tm.hideObjects();
    const final = tm.getConf();
    const packed = { pack: await createPackSection(final), ...final };
    const compressed = new TiledMapCompressed(packed);
    compressed.compressLayers();
    return compressed.getConf();
}

function getTweet(id: string) {
    const rows = db.queryEntries<{ id: number; tweet_text: string }>(
        "SELECT * FROM tweets WHERE tweet_id=?",
        [id]
    );
    return rows[0].tweet_text.replaceAll(/@\w+/g, "");
}

function getCIDRows(db: DB, cid: number) {
    return db.query<[string]>(
        "SELECT tweet_id from tweet_threads where convo_id=? order by position asc",
        [cid]
    );
}

function getTweetText(db: DB, id: string) {
    try {
        return db.query<[string, string]>(
            "SELECT tweet_text,author_id from tweets where tweet_id=?",
            [id]
        )[0];
    } catch (err) {
        throw Error(`Error getting tweet text, nonexistent id? ${err}`);
    }
}
export function getConvoIDs(dbb: DB) {
    return dbb.query<[number]>("SELECT DISTINCT convo_id FROM tweet_threads;");
}

export function getConvo(dbb: DB, convo: number): [string, string][] {
    const rows = getCIDRows(dbb, convo);
    const convos = rows
        .map((id) => getTweetText(dbb, id[0]))
        .map<[string, string]>((r) => {
            let [text, author] = r;
            text = text.replaceAll(/@\w+/g, "");
            text = text.replaceAll(/https?:\/\/\S+/g, "");
            text = text.trim();
            return [text, author];
        })
        .filter((text) => text[0].trim().length > 0);
    return convos;
}
