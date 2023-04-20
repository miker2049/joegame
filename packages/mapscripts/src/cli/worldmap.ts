// -*- lsp-enabled-clients: (deno-ls); -*-
import { DB } from "https://deno.land/x/sqlite/mod.ts";
import { finalizeTiledmap, getConvo, getConvoIDs } from "../utils.ts";

import {
    WorldGenerator,
    HashObjects,
    ObjectPopulatorSystem,
    cliffSystemFromConfig,
} from "../WorldGenerator.ts";
import { TiledMap } from "../TiledMap.ts";

function getTweetRows(limit: number) {
    const db = new DB("jdb.db", { mode: "read" });
    const rows = db.queryEntries("SELECT * FROM tweets LIMIT ?", [limit]);
    db.close();
    return rows;
}

type argsType = {
    conf: string;
    stamps: string;
    out: string;
    x: number;
    y: number;
    w: number;
    h: number;
};

export async function genTilemap(conf: argsType) {
    const db = new DB("jdb.db", { mode: "read" });
    const tweets = getConvoIDs(db)
        // .slice(0, 600)
        .map((id) => getConvo(db, id[0]))
        .map((cnv) => ({ type: "convo", name: "convo", convo: cnv }));
    // Read in configuration of world
    const settings = JSON.parse(await Deno.readTextFile(conf.conf));
    // Load the wang tilemap
    const tm = new TiledMap(JSON.parse(await Deno.readTextFile(conf.stamps)));
    const wg = new WorldGenerator(tm, settings);
    const cs = cliffSystemFromConfig(settings, tm);
    // console.log(tweets);
    // add all systems
    wg.addSystem(cs);
    wg.addSystem(new HashObjects(cs, settings));
    wg.addSystem(new ObjectPopulatorSystem(tweets, [0, 0]));

    const map = await wg.getMap(conf.x, conf.y, conf.w, conf.h);
    const final = await finalizeTiledmap(map);

    Deno.writeTextFileSync(conf.out, JSON.stringify(final));
    db.close();
}

// await genTilemap(
//     Deno.args[0],
//     Deno.args[1],
//     Deno.args[2],
//     parseInt(Deno.args[3]),
//     parseInt(Deno.args[4]),
//     parseInt(Deno.args[5]),
//     parseInt(Deno.args[6])
// );
