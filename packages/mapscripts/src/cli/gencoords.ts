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

export function genCoords(c: {
    origin: [number, number];
    minDist: number;
    density: number;
    table: string;
}) {
    const db = new DB(Deno.env.get("BO_PATH"));

    db.execute(`DROP TABLE IF EXISTS ${c.table}`);
    db.execute(
        `CREATE TABLE IF NOT EXISTS ${c.table}
(
id INTEGER PRIMARY KEY AUTOINCREMENT,
x INTEGER, y INTEGER, convo_id TEXT,
UNIQUE(x,y)
)`
    );

    const cids = getNonBulkConvos(db);
    console.log("got cids");

    const convos = cids.map((cid) =>
        getConvo(db, cid[0]).map((c) => [...c, cid[0]])
    );
    console.log("got convos");
    const coords = genPolarCoords(
        convos.length,
        c.origin,
        c.minDist,
        c.density
    );
    console.log("got polar coords");
    convos.forEach((line, idx) => {
        db.query(`INSERT INTO ${c.table} (x, y, convo_id) VALUES (?, ?, ?)`, [
            coords[idx][0],
            coords[idx][1],
            line[0][2],
        ]);
    });
    console.log(`Inserted ${convos.length} rows into table ${c.table}. bye!`);
    db.close();
}
