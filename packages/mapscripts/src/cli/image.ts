// -*- lsp-enabled-clients: (deno-ls); -*-
import {
    cliffSystemFromConfig,
    mapCliffPicture,
    WorldGenerator,
} from "../WorldGenerator.ts";
import { TiledMap } from "../TiledMap.ts";
import { createCanvas } from "https://deno.land/x/canvas@v1.4.1/mod.ts";

type argsType = {
    conf: string;
    stamps: string;
    out: string;
    x: number;
    y: number;
    w: number;
    h: number;
};

export async function mapIt(conf: argsType) {
    const settings = JSON.parse(await Deno.readTextFile(conf.conf));
    // Load the wang tilemap
    const tm = new TiledMap(JSON.parse(await Deno.readTextFile(conf.stamps)));
    const cs = cliffSystemFromConfig(settings, tm);
    const cnv = createCanvas(conf.w, conf.h);
    const ctx = cnv.getContext("2d");
    if (ctx) {
        await mapCliffPicture(
            cs,
            conf.x,
            conf.y,
            conf.w,
            conf.h,
            ctx,
            settings
        );
        // cnv.save(outpath);
        await Deno.writeFile(conf.out, cnv.toBuffer());
    }
}
// mapIt(
//     Deno.args[0],
//     Deno.args[1],
//     Deno.args[2],
//     parseInt(Deno.args[3]),
//     parseInt(Deno.args[4]),
//     parseInt(Deno.args[5]),
//     parseInt(Deno.args[6])
// );
