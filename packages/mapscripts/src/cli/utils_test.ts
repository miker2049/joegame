// -*- lsp-enabled-clients: (deno-ls); -*-
import { DB } from "https://deno.land/x/sqlite/mod.ts";
import { getConvo } from "./utils.ts";
Deno.test("getConvos", () => {
    const BASEDIR = "/home/mik/joegame/assets";
    const db = new DB(BASEDIR + "/jdb.db", { mode: "read" });
    getConvo(db, 180);
    db.close();
});
