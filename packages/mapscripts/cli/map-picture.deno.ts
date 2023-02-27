// -*- lsp-enabled-clients: (deno-ls); -*-
import {
    mapCliffPicture,
    worldFromConfig,
    WangLayer,
    VoronoiManhattan,
} from "../esm/WorldGenerator.js";
import { TiledMap } from "../esm/TiledMap.js";
import { createCanvas } from "https://deno.land/x/canvas@v1.4.1/mod.ts";
// import { createCanvas } from "https://deno.land/x/skia_canvas@0.3.0/mod.ts";
async function mapIt(
    confpath: string,
    mappath: string,
    outpath: string,
    x: number,
    y: number,
    w: number,
    h: number
) {
    const conf = JSON.parse(await Deno.readTextFile(confpath));
    const tm = new TiledMap(JSON.parse(await Deno.readTextFile(mappath)));
    const i = worldFromConfig(conf, tm);

    const cnv = createCanvas(w, h);
    const ctx = cnv.getContext("2d");
    if (ctx) {
        await mapCliffPicture(i, x, y, w, h, ctx, conf);
        // cnv.save(outpath);
        await Deno.writeFile(outpath, cnv.toBuffer());
    }
}
mapIt(
    Deno.args[0],
    Deno.args[1],
    Deno.args[2],
    parseInt(Deno.args[3]),
    parseInt(Deno.args[4]),
    parseInt(Deno.args[5]),
    parseInt(Deno.args[6])
);
