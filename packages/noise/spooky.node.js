import fs from "fs";
import { Spooky } from "./spooky-wrapper";
import ww from "./spooky.wasm";

export function initSpookySync() {
    const wasmBytes = fs.readFileSync("./spooky.wasm").buffer;
    const wasmModule = new WebAssembly.Module(wasmBytes);
    const wasmInstance = new WebAssembly.Instance(wasmModule);
    return new Spooky(wasmInstance);
}
export const spooky = initSpookySync();
export function xyhash(x, y, seed = 0) {
    const xy = Number((x * 0x1f1f1f1f) ^ y).toString();
    return spooky.hash128(xy, seed);
}

export function jprng(x, y, n = 0, seed = 0) {
    const h = xyhash(x, y, seed);
    return Number("0x" + h[n] + h[n + 1]) / 255;
}
export function jprng2(x, y, n = 0, seed = 0) {
    const h = xyhash(x, y, seed);
    return [
        Number("0x" + h[n] + h[n + 1]) / 255,
        Number("0x" + h[n + 2] + h[n + 3]) / 255,
    ];
}
