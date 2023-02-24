// -*- lsp-enabled-clients: (deno-ls); -*-

import {
    worldFromConfig,
    SignalMaskFilter,
    EdgeFilter,
    BinaryFilter,
} from "../../esm/WorldGenerator.js";
import { TiledMap } from "../../esm/TiledMap.js";
import { Grid } from "../../esm/utils.js";
import { saturateObjects, createPackSection } from "../../esm/saturator.js";
import TiledRawJSON from "../../../joegamelib/src/types/TiledRawJson.d.ts";

import * as path from "https://deno.land/std@0.97.0/path/mod.ts";

const BASEDIR = "/home/mik/joegame/assets";
const IMGDIR = BASEDIR + "/images/";

function embedTilesetsOffline(map: TiledRawJSON): TiledRawJSON {
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
    const cs = worldFromConfig(conf, tm);
    // Going to collect a grid from each layer
    const allLayers: Grid[] = [];
    // Each div will be a group
    const alts = cs.getDivs();
    for (let i = alts - 1; i >= 0; i--) {
        // This gives us an array of WangLayers
        const g = cs.getLayerGroup(i);
        if (g) {
            // the "last" one in a group is always cliffs
            g[g.length - 1].mask.filters.push(new BinaryFilter(i / alts));
            for (let l = g.length - 1; l >= 0; l--) {
                allLayers.push(g[l].getTilesRect(x, y, w, h));
            }
        }
    }
    // create the new map
    const newMap = TiledMap.createEmpty(h * 4, w * 4, tm.getConf());
    allLayers.push(cs.extraLayers[0].getTilesRect(x, y, w, h));
    newMap.applyLgs(allLayers.reverse(), "gen");
    let rawMap = newMap.getConf();
    rawMap = embedTilesetsOffline(rawMap);
    rawMap = saturateObjects(rawMap);
    const final = { pack: createPackSection(rawMap), ...rawMap };
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
