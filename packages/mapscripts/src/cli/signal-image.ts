import {
    BinaryFilter,
    CircleFilter,
    Perlin,
    Signal,
    SignalConfig,
} from "../WorldGenerator.ts";
import { encode } from "https://deno.land/x/pngs/mod.ts";

function concatTypedArrays(a: Uint8Array, b: Uint8Array) {
    // a, b TypedArray of same type
    const c = new Uint8Array(a.length + b.length);
    c.set(a, 0);
    c.set(b, a.length);
    return c;
}

export async function signalImage(c: {
    sig: Signal;
    x: number;
    y: number;
    w: number;
    h: number;
    out: string;
}) {
    const img = new Uint8Array(c.w * c.h * 4);
    for (let y = 0; y < c.h; y++) {
        for (let x = 0; x < c.w; x++) {
            const val = c.sig.get(x + c.x, y + c.y);
            const o = (x + y * c.w) * 4;
            for (let p = 0; p < 3; p++) {
                if (val) img[o + p] = 0;
                else img[o + p] = 255;
            }
            img[o + 3] = 255;

            // if (val)
            //     img = concatTypedArrays(img, new Uint8Array([0, 0, 0, 255]));
            // else
            //     img = concatTypedArrays(
            //         img,
            //         new Uint8Array([255, 255, 255, 255])
            //     );
        }
    }
    const png = encode(img, c.w, c.h);
    await Deno.writeFile(c.out, png);
}

// const per = new Perlin(0.1, 30, 420, [new BinaryFilter(0.5)]);
const per = new Perlin(0.0008, 12, 420, [
    new CircleFilter(1000, 500, 500, -0.4),
    new CircleFilter(1750, 0, 500, -0.4),
    new BinaryFilter(0.5),
]);

await signalImage({
    sig: per,
    x: 0,
    y: 0,
    w: 2000,
    h: 1000,
    out: "sig.png",
});
console.log("done");
