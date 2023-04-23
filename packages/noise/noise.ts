import { perlin2d } from "./perlin";
import { Spooky } from "./spooky";
// @ts-ignore
import spookyBytes from "./spooky.wasm";
// @ts-ignore
import simplexWasm from "./simplex.wasm";

const simplexModule = new WebAssembly.Module(simplexWasm as Uint8Array);
const instance = new WebAssembly.Instance(simplexModule as Uint8Array);
const __simplex = instance.exports.simplex as (
    x: number,
    y: number,
    seed: number,
    octaves: number,
    width: number,
    amp: number,
    lacuna: number,
    persistence: number
) => number;
const simplex = (
    x: number,
    y: number,
    seed: number,
    freq = 1,
    octaves = 7,
    width = 1,
    amp = 1,
    lacuna = 2,
    persist = 0.5
) => __simplex(x * freq, y * freq, seed, octaves, width, amp, lacuna, persist);

function initSpookySync() {
    const wasmModule = new WebAssembly.Module(spookyBytes);
    const wasmInstance = new WebAssembly.Instance(wasmModule);
    return new Spooky(wasmInstance);
}
export const spooky = initSpookySync();
function xyhash(x: number, y: number, seed = 0) {
    const xy = Number((x * 0x1f1f1f1f) ^ y).toString();
    return spooky.hash128(xy, seed);
}

const hash128 = (s: string, seed = 0) => spooky.hash128(s, 0);

const OFF = 3;

function jprng(x: number, y: number, n = 0, seed = 0) {
    const h = xyhash(x, y, seed);
    return Number("0x" + h[n + OFF] + h[n + 1 + OFF]) / 255;
}
function jprng2(x: number, y: number, n = 0, seed = 0) {
    const h = xyhash(x, y, seed);
    return [
        Number("0x" + h[n + OFF] + h[n + 1 + OFF]) / 255,
        Number("0x" + h[n + 2 + OFF] + h[n + 3 + OFF]) / 255,
    ];
}

export { xyhash, jprng, jprng2, simplex, hash128, perlin2d };
