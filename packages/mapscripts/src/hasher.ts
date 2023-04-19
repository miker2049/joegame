// -*- lsp-enabled-clients: (deno-ls); -*-
// import { xyhash, jprng, jprng2 } from "../../noise/ripemd160.ts";
// export { xyhash, jprng, jprng2 };
import { Hashes } from "../../noise/jshashes.js";
import xxhash from "https://unpkg.com/xxhash-wasm/esm/xxhash-wasm.js";
const { h64ToString } = await xxhash();
const h = new Hashes.MD5();
export function xyhash(x: number, y: number, seed = "") {
    const xy = `${x}--${y}` + seed;
    return h64ToString(xy);
}

export function jprng(x: number, y: number, n = 0, seed = "") {
    const h = xyhash(x, y, seed);
    return Number("0x" + h[n] + h[n + 1]) / 255;
}
export function jprng2(x: number, y: number, n = 0, seed = "") {
    const h = xyhash(x, y, seed);
    return [
        Number("0x" + h[n] + h[n + 1]) / 255,
        Number("0x" + h[n + 2] + h[n + 3]) / 255,
    ];
}
