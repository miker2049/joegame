// -*- lsp-enabled-clients: (deno-ls); -*-
import { genPolarCoords } from "../esm/utils.js";

/**
 * Prepend rows of CSV found at csvpath based on our special
 * mechanism getQuadForObject.  This will place an object based on its index and a saturation level,
 * such that objects will be placed randomly starting around the origin and radiating outwards.
 *
 * foreignTable and foreignColumn give the table to get our list of objects from,
 * and what column to add to the table.
 *
 * limit is the cutoff of how many objects to get from db
 *
 * quadSize is the working size of quads, (which is not necessarily the same as wangQuads!)
 */
function genCoords(csvpath: string, outpath: string, origin: [number, number]) {
    const data = Deno.readTextFileSync(csvpath).split("\n");
    const coords = genPolarCoords(data.length, origin, 64, 128);
    const coorded = data
        .map((line, idx) => {
            return `${coords[idx][0]},${coords[idx][1]},` + line;
        })
        .join("\n");
    Deno.writeTextFileSync(outpath, coorded);
}

genCoords(
    Deno.args[0],
    Deno.args[1],
    Deno.args[2].split(",").map((i) => Number(i)) as [number, number]
);
