// -*- lsp-enabled-clients: (deno-ls); -*-
import { DB } from "https://deno.land/x/sqlite/mod.ts";

function getMaxThreads(db: DB): number {
    return db.query<number[]>("SELECT max(convo_id) from tweet_threads")[0][0];
}

function getCIDRows(db: DB, cid: number) {
    return db.query<[number, number, number, string]>(
        "SELECT * from tweet_threads where convo_id=?",
        [cid]
    );
}

function getAlterations(db: DB): [number, number][] {
    // const db = new DB(dbpath);
    // Get a quad for an object: getQuadForObject(saturation, index)
    // Create a new table called coordTable if it doesn't already exist

    // Get an array of objects to generate coordinates for from the foreign table
    const currMax = Number(getMaxThreads(db));
    let maxOff = 1;
    const fixes: [number, number][] = [];
    for (let currCid = 1; currCid < currMax; currCid += 1) {
        const oldThread = getCIDRows(db, currCid).sort((a, b) => a[0] - b[0]);
        let counter = 0;
        const chunks: [number, number, number, string][][] = [];
        for (const row in oldThread) {
            const last = oldThread[Math.max(0, Number(row) - 1)];
            const curr = oldThread[row];
            if (curr[0] - last[0] > 1) counter += 1;
            if (!chunks[counter]) chunks[counter] = [];
            chunks[counter].push(curr);
        }

        if (chunks.length > 1) {
            tt += 1;
            for (let i = 1; i < chunks.length; i++) {
                const newIdx = currMax + maxOff;
                chunks[i].forEach((row) => {
                    fixes.push([row[0], newIdx]);
                });
                maxOff += 1;
            }
        }
    }
    return fixes;
}

function updateCID(db: DB, id: number, cid: number) {
    db.query("UPDATE tweet_threads set convo_id = ? where id = ?", [cid, id]);
}

function fix(dbpath: string) {
    const db = new DB(dbpath);
    const alte = getAlterations(db);
    alte.forEach((a) => updateCID(db, a[0], a[1]));
    // console.log(alte);
    db.close();
}

fix(Deno.args[0]);
