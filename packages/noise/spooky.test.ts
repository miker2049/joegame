import test from "tape";
import fs from "fs";
import { Spooky } from "./spooky-wrapper";

export function initSpookySync() {
    const wasmBytes = fs.readFileSync("spooky.wasm").buffer;
    const wasmModule = new WebAssembly.Module(wasmBytes);
    const wasmInstance = new WebAssembly.Instance(wasmModule);
    // console.log(all);
    return new Spooky(wasmInstance);
}
test("spooky hash works", (t) => {
    const s = initSpookySync();
    // s.hash128("hfsadasdasdas", 109);
    // s.hash128("h", 1011);
    // s.hash128("h", 109);
    // s.hash128("h", 109);
    // s.hash128("hey", 109);
    // s.hash128("hfy", 109);
    // s.hash128("hey", 109);

    // s.hash128("h", 109);
    // s.hash128("he", 109);
    // s.hash128("hel", 1090);
    // s.hash128("hl", 109);
    // s.hash128("hl", 109);
    // s.hash128("hlgg", 109);
    // s.hash128("hl123", 109);
    s.hash128(
        "hl123wkdjalkdjwadlkajdwawlkdjawbdlkawjbalckasjcnsalkjdnalksjcnaskclaskjc\nasdjlaskjdaslkjdaslkdjasdhlaksjdashlkdjaslkdjashldkasjdhlaskjdaslkdjaslbclaksjdhalskdjascblask",
        109
    );
    s.hash128(
        "hl123wkdjalkdjwadlkajdwawlkdjawbdlkawjbalckasjcnsalkjdnalksjcnaskclaskjc\nasdjlaskjdaslkjdaslkdjasdhlaksjdashlkdjaslkdjashldkasjdhlaskjdaslkdjaslbclaksjdhalskdjascblasf",
        109
    );

    s.hash128(
        "hl123wkdjalkdjwadlkajdwawlkdjawbdlkawjbalckasjcnsalkjdnalksjcnaskclaskjc\nasdjlaskjdaslkjdaslkdjasdhlaksjdashlkdjaslkdjashldkasjdhlaskjdaslkdjaslbclaksjdhalskdjascblasf",
        108
    );
    t.end();
});
