// -*- lsp-enabled-clients: (deno-ls); -*-
import { DB } from "https://deno.land/x/sqlite/mod.ts";
import { finalizeTiledmap, getConvo, getConvoIDs } from "./utils.ts";

import {
    WorldGenerator,
    HashObjects,
    ObjectPopulatorSystem,
    cliffSystemFromConfig,
} from "../src/WorldGenerator.ts";
import { TiledMap } from "../src/TiledMap.ts";

function getTweetRows(limit: number) {
    const db = new DB("jdb.db", { mode: "read" });
    const rows = db.queryEntries("SELECT * FROM tweets LIMIT ?", [limit]);
    db.close();
    return rows;
}

async function genTilemap(
    confpath: string,
    mappath: string,
    outpath: string,
    x: number,
    y: number,
    w: number,
    h: number
) {
    const db = new DB("jdb.db", { mode: "read" });
    const tweets = getConvoIDs(db)
        // .slice(0, 600)
        .map((id) => getConvo(db, id[0]))
        .map((cnv) => ({ type: "convo", convo: cnv }));
    // Read in configuration of world
    const conf = JSON.parse(await Deno.readTextFile(confpath));
    // Load the wang tilemap
    const tm = new TiledMap(JSON.parse(await Deno.readTextFile(mappath)));
    const wg = new WorldGenerator(tm, conf);
    const cs = cliffSystemFromConfig(conf, tm);
    // console.log(tweets);
    // add all systems
    wg.addSystem(cs);
    wg.addSystem(new HashObjects(cs, conf));
    wg.addSystem(new ObjectPopulatorSystem(tweets, [0, 0]));

    const map = await wg.getMap(x, y, w, h);
    const final = await finalizeTiledmap(map);

    Deno.writeTextFileSync(outpath, JSON.stringify(final));
    db.close();
}

await genTilemap(
    Deno.args[0],
    Deno.args[1],
    Deno.args[2],
    parseInt(Deno.args[3]),
    parseInt(Deno.args[4]),
    parseInt(Deno.args[5]),
    parseInt(Deno.args[6])
);
