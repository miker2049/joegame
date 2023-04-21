// -*- lsp-enabled-clients: (deno-ls); -*-
import { genPolarCoords, getConvo, getNonBulkConvos } from "../utils.ts";
import { DB } from "https://deno.land/x/sqlite@v3.7.0/mod.ts";

const LIMIT = 100;
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
function genCoords(outpath: string, origin: [number, number]) {
    const db = new DB(Deno.env.get("BO_PATH"));
    const cids = getNonBulkConvos(db);
    console.log("got cids");
    const convos = cids
        .map((cid) => getConvo(db, cid[0]))
        .map((row) => row.join(","));
    console.log("got convos");
    const coords = genPolarCoords(convos.length, origin, 64, 128);
    console.log("got polar coords");
    const coorded = convos
        .map((line, idx) => {
            return `${coords[idx][0]},${coords[idx][1]},` + line;
        })
        .join("\n");
    Deno.writeTextFileSync(outpath, coorded);
}

genCoords(
    Deno.args[0],
    Deno.args[1].split(",").map((i) => Number(i)) as [number, number]
);
