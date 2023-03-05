import pako from "pako";

export function parseCompressed(input: string) {
    try {
        const decoded = new Uint8Array(Buffer.from(input, "base64"));
        const result = pako.inflate(decoded);
        const arr = new Int32Array(result.buffer);
        return arr;
    } catch (err) {
        console.log(err);
    }
}

// parseCompressed(
//     "eAHtmztuAzEQQ10FCJD73yNOrpDPsSK16mQG9Ip8hQoVg9FwnoerNfb+crt9jPU51vtYd/ZbenwPvX7G+h3rayz2e3rAm/b7g7c93la94A/+njm/4Q/+4O/c58/VT9jv+THzj/nH/GP+td6fmX/MP+Yf84/5x/vnR96/c9/Yu2+seuG/+C/+i//iv/gv/uv//xr/xX/xX/wX/8V/8V/897TngfV9Avu99zGn9ftq54W3Pd5Wva7Wz9POs+rJfo/H/+j32+u59we1fpW3qd0z75/q+dV4Rf+p3VzN348o+k/tlPiE701U/pT4R+6bV8un8AN/2vduU7+r8eA+D/xpzw/ufqXlU/hL8E+1/jQe3PWo+rfHu/uVlq+dH7X+NB7c9aj6t8e7+5WWr50ftf40Htz1qPq3x7v7lZavnR+1/jQe3PWo+rfHu/uVlq+dH7X+NB7c9aj6t8e7+5WWr50ftf40Htz1qPq3x7v7lZavnR+1/jQe3PWo+rfHu/uVlq+dH7X+NB7c9aj6t8e7+5WWr50ftf4/vIH5iQ=="
// );
