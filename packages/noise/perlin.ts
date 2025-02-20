// converted from common lisp found here https://gist.github.com/nowl/828707
const NOISE_HASH = [
    208, 34, 231, 213, 32, 248, 233, 56, 161, 78, 24, 140, 71, 48, 140, 254,
    245, 255, 247, 247, 40, 185, 248, 251, 245, 28, 124, 204, 204, 76, 36, 1,
    107, 28, 234, 163, 202, 224, 245, 128, 167, 204, 9, 92, 217, 54, 239, 174,
    173, 102, 193, 189, 190, 121, 100, 108, 167, 44, 43, 77, 180, 204, 8, 81,
    70, 223, 11, 38, 24, 254, 210, 210, 177, 32, 81, 195, 243, 125, 8, 169, 112,
    32, 97, 53, 195, 13, 203, 9, 47, 104, 125, 117, 114, 124, 165, 203, 181,
    235, 193, 206, 70, 180, 174, 0, 167, 181, 41, 164, 30, 116, 127, 198, 245,
    146, 87, 224, 149, 206, 57, 4, 192, 210, 65, 210, 129, 240, 178, 105, 228,
    108, 245, 148, 140, 40, 35, 195, 38, 58, 65, 207, 215, 253, 65, 85, 208, 76,
    62, 3, 237, 55, 89, 232, 50, 217, 64, 244, 157, 199, 121, 252, 90, 17, 212,
    203, 149, 152, 140, 187, 234, 177, 73, 174, 193, 100, 192, 143, 97, 53, 145,
    135, 19, 103, 13, 90, 135, 151, 199, 91, 239, 247, 33, 39, 145, 101, 120,
    99, 3, 186, 86, 99, 41, 237, 203, 111, 79, 220, 135, 158, 42, 30, 154, 120,
    67, 87, 167, 135, 176, 183, 191, 253, 115, 184, 21, 233, 58, 129, 233, 142,
    39, 128, 211, 118, 137, 139, 255, 114, 20, 218, 113, 154, 27, 127, 246, 250,
    1, 8, 198, 250, 209, 92, 222, 173, 21, 88, 102, 219,
];

const SEED = 0;

function noise2(x: number, y: number, seed: number, hash = NOISE_HASH) {
    return hash[(x + hash[(y + seed) % 256]) % 256];
}

function linearInterpolation(x: number, y: number, s: number) {
    return (y - x) * s + x;
}

function smoothInterpolation(x: number, y: number, s: number) {
    return linearInterpolation(x, y, s * s * (3 - 2 * s));
}

function truncate2(n: number) {
    const integer = n | 0;
    let fraction = 0;
    if (!Number.isInteger(n)) fraction = Math.abs(n - integer);
    return [integer, fraction];
}

function truncate(n: number) {
    let f = Math.floor(n);
    let split = `${n}`.split(".");
    let frac = 0;
    if (split.length === 2) frac = parseFloat("0." + split[1]);
    const out = [f, frac];
    return out;
}

function noise2d(x: number, y: number, seed: number) {
    const [x0, x0frac] = truncate(x),
        [y0, y0frac] = truncate(y);
    const x1 = x0 + 1,
        y1 = y0 + 1;
    const a = noise2(x0, y0, seed),
        b = noise2(x1, y0, seed),
        c = noise2(x0, y1, seed),
        d = noise2(x1, y1, seed);
    const low = smoothInterpolation(a, b, x0frac);
    const high = smoothInterpolation(c, d, x0frac);
    const out = smoothInterpolation(low, high, y0frac);
    if (isNaN(out)) debugger;
    return out;
}

export function perlin2d(
    x: number,
    y: number,
    freq = 0.03,
    depth = 10,
    seed = 0
) {
    let xn = x * freq,
        yn = y * freq,
        final = 0,
        amp = 1,
        div = 0;
    for (let d = 0; d < depth; d++) {
        const val = noise2d(xn, yn, seed);
        div = div + 256 * amp;
        final = final + amp * val;
        xn = 2 * xn;
        yn = 2 * yn;
        amp = amp / 2;
    }
    return final / div;
}

export function perlin2dGrid(
    width: number,
    height: number,
    freq?: number,
    depth?: number,
    seed?: number
) {
    const res: number[][] = [];
    for (let y = 0; y < height; y++) {
        res[y] = [];
        for (let x = 0; x < width; x++) {
            const out = perlin2d(x, y, freq, depth, seed);
            res[y][x] = out;
        }
    }
    return res;
}
