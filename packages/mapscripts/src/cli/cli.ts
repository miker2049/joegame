import { parse } from "https://deno.land/std@0.184.0/flags/mod.ts";
import { relative } from "https://deno.land/std@0.184.0/path/mod.ts";
import { mapIt } from "./image.ts";
import { saturateMap } from "../saturator.ts";
import { genTilemap } from "./worldmap.ts";
import { genCoords } from "./gencoords.ts";

const userMode = Deno.args[0];
const args = parse(Deno.args.slice(1));

const STAMPS = "/joegame/assets/maps/desert-stamps2.json";
const OUT_MAP = "/joegame/assets/maps/def.json";
const OUT_IMAGE = "map.png";
const CONF = "/joegame/packages/mapscripts/src/world-settings.json";

// Assuming joegame is right in HOME dir
const relPath = (p: string) => relative(Deno.cwd(), Deno.env.get("HOME") + p);

switch (userMode) {
    case "image": {
        const finalConfig = Object.assign(
            {
                x: 0,
                y: 0,
                w: 500,
                h: 500,
                conf: relPath(CONF),
                out: OUT_IMAGE,
                stamps: relPath(STAMPS),
            },
            args
        );
        console.log(`
************************************
Creating map image
************************************
x: ${finalConfig.x}
y: ${finalConfig.y}
w: ${finalConfig.w}
h: ${finalConfig.h}

Stamps file: ${finalConfig.stamps}
World Config: ${finalConfig.conf}
Out file: ${finalConfig.out}
`);
        mapIt(finalConfig);
        break;
    }
    case "saturate": {
        const finalConfig = Object.assign(
            {
                i: "inmap",
                o: "saturated.json",
            },
            args
        );
        console.log(`
************************************
Saturating Tiled file
************************************
In file: ${finalConfig.i}
Out file: ${finalConfig.o}
`);

        if (!(await Deno.stat(finalConfig.i))) {
            console.log("Invalid infile specified!");
        } else {
            const tm = JSON.parse(await Deno.readTextFile(finalConfig.i));
            const t = await saturateMap(tm);
            await Deno.writeTextFile(finalConfig.o, JSON.stringify(t));
        }
        break;
    }
    case "map": {
        const finalConfig = Object.assign(
            {
                x: 0,
                y: 0,
                w: 40,
                h: 40,
                conf: relPath(CONF),
                out: relPath(OUT_MAP),
                stamps: relPath(STAMPS),
            },
            args
        );
        console.log(`
************************************
Creating Tiled Map
************************************
x: ${finalConfig.x}
y: ${finalConfig.y}
w: ${finalConfig.w}
h: ${finalConfig.h}

Stamps file: ${finalConfig.stamps}
World Config: ${finalConfig.conf}
Out file: ${finalConfig.out}
`);
        genTilemap(finalConfig);
        break;
    }
    case "coords": {
        const c = Object.assign(
            {
                origin: [5000, 5000] as [number, number],
                minDist: 64,
                density: 128,
                table: "convo_coords",
            },
            args
        );
        console.log(`
************************************
Creating coords for convos
************************************
origin: ${c.origin}
minDist: ${c.minDist}
density: ${c.density}
table: ${c.table}
`);
        genCoords(c);
        break;
    }
    default:
        console.log(
            "You need to select a valid mode: image, map, saturate, coords"
        );
}
