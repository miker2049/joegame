// -*- lsp-enabled-clients: (deno-ls); -*-

import {
    worldFromConfig,
    WorldGenerator,
    SignalMaskFilter,
    EdgeFilter,
    BinaryFilter,
} from "../esm/WorldGenerator.js";
import { TiledMap } from "../esm/TiledMap.js";
import { finalizeTiledmap } from "./utils.ts";
import * as path from "https://deno.land/std@0.178.0/path/mod.ts";
async function genTilemap(
    confpath: string,
    mappath: string,
    outpath: string,
    x: number,
    y: number,
    w: number,
    h: number
) {
    // Read in configuration of world
    const conf = JSON.parse(await Deno.readTextFile(confpath));
    // Load the wang tilemap
    const tm = new TiledMap(JSON.parse(await Deno.readTextFile(mappath)));
    // Generate system
    // const cs = worldFromConfig(conf, tm);
    // // Going to collect a grid from each layer
    // const allLayers = cs.getAllTileLayers(x, y, w, h);

    // // create the new map
    // const newMap = TiledMap.createEmpty(h * 4, w * 4, tm.getConf());
    const wg = new WorldGenerator(tm, conf);
    const final = finalizeTiledmap(
        wg.getMap(x, y, w, h),
        path.resolve(mappath)
    );

    Deno.writeTextFileSync(outpath, JSON.stringify(final));
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
