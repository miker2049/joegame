import { createHash } from "node:crypto";

export function xyhash(x, y, seed = 0) {
    const xy = Number((x * 0x1f1f1f1f) ^ y).toString();
    const b = createHash("blake2s256");
    b.update(xy + seed);
    return b.digest("hex");
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
