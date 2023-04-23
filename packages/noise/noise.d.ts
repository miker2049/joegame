declare module "perlin" {
    export function perlin2d(x: number, y: number, freq?: number, depth?: number, seed?: number): number;
    export function perlin2dGrid(width: number, height: number, freq?: number, depth?: number, seed?: number): number[][];
}
declare module "spooky" {
    export class Spooky {
        expts: WebAssembly.Exports;
        memory: WebAssembly.Memory;
        malloc: (p: number) => number;
        free: (p: number) => void;
        decoder: TextDecoder;
        encoder: TextEncoder;
        constructor(wasmMod: WebAssembly.Instance);
        hash64(msg: string, seed: number): string;
        hash128(msg: string, seed: number): string;
        _hashfunc(msg: string, seed: number, func: any): string;
        ptr2str(ptr: number, length: number): string;
        str2ptr(s: string): Uint8Array;
    }
}
declare module "noise" {
    import { perlin2d } from "perlin";
    import { Spooky } from "spooky";
    const simplex: (x: number, y: number, seed: number, freq?: number, octaves?: number, width?: number, amp?: number, lacuna?: number, persist?: number) => number;
    export const spooky: Spooky;
    function xyhash(x: number, y: number, seed?: number): string;
    const hash128: (s: string, seed?: number) => string;
    function jprng(x: number, y: number, n?: number, seed?: number): number;
    function jprng2(x: number, y: number, n?: number, seed?: number): number[];
    export { xyhash, jprng, jprng2, simplex, hash128, perlin2d };
}
