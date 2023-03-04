import test from "tape";
import fs from "fs/promises";
import { Spooky } from "./spooky.node";

async function initSpooky() {
    const wasmBytes = await fs.readFile("spooky.wasm");
    return new Spooky(await WebAssembly.instantiate(wasmBytes));
}
test("spooky hash works", (t) => {
    initSpooky().then((s) => {
        s.hash128("hfsadasdasdas", 109);
        s.hash128("h", 1011);
        s.hash128("h", 109);
        s.hash128("h", 109);
        s.hash128("hey", 109);
        s.hash128("hfy", 109);
        s.hash128("hey", 109);

        s.hash128("h", 109);
        s.hash128("he", 109);
        s.hash128("hel", 109);
        s.hash128("hl", 109);
        t.end();
    });
});
