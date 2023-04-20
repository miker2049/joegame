// -*- lsp-enabled-clients: (deno-ls); -*-
import { getQuadForObject } from "../utils.ts";
import { jprng2 } from "../hasher.ts";
import { Database } from "https://deno.land/x/sqlite3@0.9.1/mod.ts";

/**
 * Creates a new table in db called coordTable.  Proceeds to generate rows based on our special
 * mechanism found in getQuadForObject.  This will place an object based on its index and a saturation level,
 * such that objects will be placed randomly starting around the origin and radiating outwards.
 *
 * foreignTable and foreignColumn give the table to get our list of objects from,
 * and what column to add to the table.
 *
 * limit is the cutoff of how many objects to get from db
 *
 * quadSize is the working size of quads, (which is not necessarily the same as wangQuads!)
 */
function genCoords(
    dbpath: string,
    coordTable: string,
    origin: [number, number],
    saturation: number,
    foreignTable: string,
    foreignColumn: string,
    limit: number,
    quadSize: number
) {
    const db = new Database(dbpath);
    // Get a quad for an object: getQuadForObject(saturation, index)
    // Create a new table called coordTable if it doesn't already exist
    db.run(
        "CREATE TABLE IF NOT EXISTS ? (id INTEGER PRIMARY KEY AUTOINCREMENT, x INTEGER, y INTEGER, ? INTEGER)",
        coordTable,
        foreignColumn
    );

    // Get an array of objects to generate coordinates for from the foreign table
    const stmt = db.prepare("SELECT * from ? ORDER BY id ASC LIMIT ?");
    const rows = stmt.all(foreignTable, limit);

    // Generate and insert random coordinates for each object
    rows.forEach(({ id }, idx) => {
        // The meaty part
        const [qx, qy] = getQuadForObject(saturation, idx);
        // the actual quad we need
        const quad = [origin[0] + qx, origin[1] + qy];
        // getting coords in mapspace within our decided upon coord
        const r = jprng2(quad[0], quad[1], 0, idx.toString());
        const realCoord = [
            Math.floor(r[0] * quadSize),
            Math.floor(r[1] * quadSize),
        ];
        db.run(
            `INSERT INTO ${coordTable} (x, y, ${foreignColumn}) VALUES (?, ?, ?)`,
            realCoord[0],
            realCoord[1],
            id
        );
    });

    db.close();
}
