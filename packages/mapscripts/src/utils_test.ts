// -*- lsp-enabled-clients: (deno-ls); -*-
import { DB } from "https://deno.land/x/sqlite/mod.ts";
import { getConvo, Grid } from "./utils.ts";
Deno.test("getConvos", () => {
    const BASEDIR = "/home/mik/joegame/assets";
    const db = new DB(BASEDIR + "/jdb.db", { mode: "read" });
    getConvo(db, 180);
    db.close();
});

Deno.test("Grid -- entries", () => {
    const grid2 = Grid.fromGrid([
        [0, 0, 0, 0, 0, 0],
        [0, 1, 1, 1, 0, 0],
        [0, 1, 1, 0, 0, 0],
        [0, 1, 1, 1, 0, 0],
        [0, 0, 0, 0, 1, 0],
        [0, 0, 0, 0, 0, 0],
    ]);
});
