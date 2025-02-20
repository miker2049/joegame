import { Hashes } from "./jshashes.js";
const RMD160 = new Hashes.RMD160();
/*
 * A JavaScript implementation of the RIPEMD-160 Algorithm
 * Version 2.2 Copyright Jeremy Lin, Paul Johnston 2000 - 2009.
 * Other contributors: Greg Holt, Andrew Kepert, Ydnar, Lostinet
 * Distributed under the BSD License
 * See http://pajhome.org.uk/crypt/md5 for details.
 * Also http://www.ocf.berkeley.edu/~jjlin/jsotp/
 */

/*
 * Configurable variables. You may need to tweak these to be compatible with
 * the server-side, but the defaults work in most cases.
 */
let hexcase = 0; /* hex output format. 0 - lowercase; 1 - uppercase        */
let b64pad = ""; /* base-64 pad character. "=" for strict RFC compliance   */

/*
 * These are the functions you'll usually want to call
 * They take string arguments and return either hex or base-64 encoded strings
 */
export function hex_rmd160(s: string) {
    return RMD160.hex(s);
    // return rstr2hex(rstr_rmd160(str2rstr_utf8(s)));
}
function b64_rmd160(s) {
    return rstr2b64(rstr_rmd160(str2rstr_utf8(s)));
}
function any_rmd160(s, e) {
    return rstr2any(rstr_rmd160(str2rstr_utf8(s)), e);
}
function hex_hmac_rmd160(k, d) {
    return rstr2hex(rstr_hmac_rmd160(str2rstr_utf8(k), str2rstr_utf8(d)));
}
function b64_hmac_rmd160(k, d) {
    return rstr2b64(rstr_hmac_rmd160(str2rstr_utf8(k), str2rstr_utf8(d)));
}
function any_hmac_rmd160(k, d, e) {
    return rstr2any(rstr_hmac_rmd160(str2rstr_utf8(k), str2rstr_utf8(d)), e);
}

/*
 * Perform a simple self-test to see if the VM is working
 */
function rmd160_vm_test() {
    return (
        hex_rmd160("abc").toLowerCase() ==
        "8eb208f7e05d987a9b044a8e98c6b087f15a0bfc"
    );
}

/*
 * Calculate the rmd160 of a raw string
 */
function rstr_rmd160(s) {
    return binl2rstr(binl_rmd160(rstr2binl(s), s.length * 8));
}

/*
 * Calculate the HMAC-rmd160 of a key and some data (raw strings)
 */
function rstr_hmac_rmd160(key, data) {
    let bkey = rstr2binl(key);
    if (bkey.length > 16) bkey = binl_rmd160(bkey, key.length * 8);

    let ipad = Array(16),
        opad = Array(16);
    for (let i = 0; i < 16; i++) {
        ipad[i] = bkey[i] ^ 0x36363636;
        opad[i] = bkey[i] ^ 0x5c5c5c5c;
    }

    let hash = binl_rmd160(ipad.concat(rstr2binl(data)), 512 + data.length * 8);
    return binl2rstr(binl_rmd160(opad.concat(hash), 512 + 160));
}

/*
 * Convert a raw string to a hex string
 */
function rstr2hex(input) {
    try {
        hexcase;
    } catch (e) {
        hexcase = 0;
    }
    let hex_tab = hexcase ? "0123456789ABCDEF" : "0123456789abcdef";
    let output = "";
    let x;
    for (let i = 0; i < input.length; i++) {
        x = input.charCodeAt(i);
        output += hex_tab.charAt((x >>> 4) & 0x0f) + hex_tab.charAt(x & 0x0f);
    }
    return output;
}

/*
 * Convert a raw string to a base-64 string
 */
function rstr2b64(input) {
    try {
        b64pad;
    } catch (e) {
        b64pad = "";
    }
    let tab =
        "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";
    let output = "";
    let len = input.length;
    for (let i = 0; i < len; i += 3) {
        let triplet =
            (input.charCodeAt(i) << 16) |
            (i + 1 < len ? input.charCodeAt(i + 1) << 8 : 0) |
            (i + 2 < len ? input.charCodeAt(i + 2) : 0);
        for (let j = 0; j < 4; j++) {
            if (i * 8 + j * 6 > input.length * 8) output += b64pad;
            else output += tab.charAt((triplet >>> (6 * (3 - j))) & 0x3f);
        }
    }
    return output;
}

/*
 * Convert a raw string to an arbitrary string encoding
 */
function rstr2any(input, encoding) {
    let divisor = encoding.length;
    let remainders = Array();
    let i, q, x, quotient;

    /* Convert to an array of 16-bit big-endian values, forming the dividend */
    let dividend = Array(Math.ceil(input.length / 2));
    for (i = 0; i < dividend.length; i++) {
        dividend[i] =
            (input.charCodeAt(i * 2) << 8) | input.charCodeAt(i * 2 + 1);
    }

    /*
     * Repeatedly perform a long division. The binary array forms the dividend,
     * the length of the encoding is the divisor. Once computed, the quotient
     * forms the dividend for the next step. We stop when the dividend is zero.
     * All remainders are stored for later use.
     */
    while (dividend.length > 0) {
        quotient = Array();
        x = 0;
        for (i = 0; i < dividend.length; i++) {
            x = (x << 16) + dividend[i];
            q = Math.floor(x / divisor);
            x -= q * divisor;
            if (quotient.length > 0 || q > 0) quotient[quotient.length] = q;
        }
        remainders[remainders.length] = x;
        dividend = quotient;
    }

    /* Convert the remainders to the output string */
    let output = "";
    for (i = remainders.length - 1; i >= 0; i--)
        output += encoding.charAt(remainders[i]);

    /* Append leading zero equivalents */
    let full_length = Math.ceil(
        (input.length * 8) / (Math.log(encoding.length) / Math.log(2))
    );
    for (i = output.length; i < full_length; i++) output = encoding[0] + output;

    return output;
}

/*
 * Encode a string as utf-8.
 * For efficiency, this assumes the input is valid utf-16.
 */
function str2rstr_utf8(input) {
    let output = "";
    let i = -1;
    let x, y;

    while (++i < input.length) {
        /* Decode utf-16 surrogate pairs */
        x = input.charCodeAt(i);
        y = i + 1 < input.length ? input.charCodeAt(i + 1) : 0;
        if (0xd800 <= x && x <= 0xdbff && 0xdc00 <= y && y <= 0xdfff) {
            x = 0x10000 + ((x & 0x03ff) << 10) + (y & 0x03ff);
            i++;
        }

        /* Encode output as utf-8 */
        if (x <= 0x7f) output += String.fromCharCode(x);
        else if (x <= 0x7ff)
            output += String.fromCharCode(
                0xc0 | ((x >>> 6) & 0x1f),
                0x80 | (x & 0x3f)
            );
        else if (x <= 0xffff)
            output += String.fromCharCode(
                0xe0 | ((x >>> 12) & 0x0f),
                0x80 | ((x >>> 6) & 0x3f),
                0x80 | (x & 0x3f)
            );
        else if (x <= 0x1fffff)
            output += String.fromCharCode(
                0xf0 | ((x >>> 18) & 0x07),
                0x80 | ((x >>> 12) & 0x3f),
                0x80 | ((x >>> 6) & 0x3f),
                0x80 | (x & 0x3f)
            );
    }
    return output;
}

/*
 * Encode a string as utf-16
 */
function str2rstr_utf16le(input) {
    let output = "";
    for (let i = 0; i < input.length; i++)
        output += String.fromCharCode(
            input.charCodeAt(i) & 0xff,
            (input.charCodeAt(i) >>> 8) & 0xff
        );
    return output;
}

function str2rstr_utf16be(input) {
    let output = "";
    for (let i = 0; i < input.length; i++)
        output += String.fromCharCode(
            (input.charCodeAt(i) >>> 8) & 0xff,
            input.charCodeAt(i) & 0xff
        );
    return output;
}

/*
 * Convert a raw string to an array of little-endian words
 * Characters >255 have their high-byte silently ignored.
 */
function rstr2binl(input) {
    let output = Array(input.length >> 2).fill(0);
    for (let i = 0; i < input.length * 8; i += 8)
        output[i >> 5] |= (input.charCodeAt(i / 8) & 0xff) << i % 32;
    return output;
}

/*
 * Convert an array of little-endian words to a string
 */
function binl2rstr(input) {
    let output = "";
    for (let i = 0; i < input.length * 32; i += 8)
        output += String.fromCharCode((input[i >> 5] >>> i % 32) & 0xff);
    return output;
}

/*
 * Calculate the RIPE-MD160 of an array of little-endian words, and a bit length.
 */
function binl_rmd160(x, len) {
    /* append padding */
    x[len >> 5] |= 0x80 << len % 32;
    x[(((len + 64) >>> 9) << 4) + 14] = len;

    let h0 = 0x67452301;
    let h1 = 0xefcdab89;
    let h2 = 0x98badcfe;
    let h3 = 0x10325476;
    let h4 = 0xc3d2e1f0;

    for (let i = 0; i < x.length; i += 16) {
        let T;
        let A1 = h0,
            B1 = h1,
            C1 = h2,
            D1 = h3,
            E1 = h4;
        let A2 = h0,
            B2 = h1,
            C2 = h2,
            D2 = h3,
            E2 = h4;
        for (let j = 0; j <= 79; ++j) {
            T = safe_add(A1, rmd160_f(j, B1, C1, D1));
            T = safe_add(T, x[i + rmd160_r1[j]]);
            T = safe_add(T, rmd160_K1(j));
            T = safe_add(bit_rol(T, rmd160_s1[j]), E1);
            A1 = E1;
            E1 = D1;
            D1 = bit_rol(C1, 10);
            C1 = B1;
            B1 = T;
            T = safe_add(A2, rmd160_f(79 - j, B2, C2, D2));
            T = safe_add(T, x[i + rmd160_r2[j]]);
            T = safe_add(T, rmd160_K2(j));
            T = safe_add(bit_rol(T, rmd160_s2[j]), E2);
            A2 = E2;
            E2 = D2;
            D2 = bit_rol(C2, 10);
            C2 = B2;
            B2 = T;
        }
        T = safe_add(h1, safe_add(C1, D2));
        h1 = safe_add(h2, safe_add(D1, E2));
        h2 = safe_add(h3, safe_add(E1, A2));
        h3 = safe_add(h4, safe_add(A1, B2));
        h4 = safe_add(h0, safe_add(B1, C2));
        h0 = T;
    }
    return [h0, h1, h2, h3, h4];
}

function rmd160_f(j, x, y, z) {
    return 0 <= j && j <= 15
        ? x ^ y ^ z
        : 16 <= j && j <= 31
        ? (x & y) | (~x & z)
        : 32 <= j && j <= 47
        ? (x | ~y) ^ z
        : 48 <= j && j <= 63
        ? (x & z) | (y & ~z)
        : 64 <= j && j <= 79
        ? x ^ (y | ~z)
        : "rmd160_f: j out of range";
}
function rmd160_K1(j) {
    return 0 <= j && j <= 15
        ? 0x00000000
        : 16 <= j && j <= 31
        ? 0x5a827999
        : 32 <= j && j <= 47
        ? 0x6ed9eba1
        : 48 <= j && j <= 63
        ? 0x8f1bbcdc
        : 64 <= j && j <= 79
        ? 0xa953fd4e
        : "rmd160_K1: j out of range";
}
function rmd160_K2(j) {
    return 0 <= j && j <= 15
        ? 0x50a28be6
        : 16 <= j && j <= 31
        ? 0x5c4dd124
        : 32 <= j && j <= 47
        ? 0x6d703ef3
        : 48 <= j && j <= 63
        ? 0x7a6d76e9
        : 64 <= j && j <= 79
        ? 0x00000000
        : "rmd160_K2: j out of range";
}
let rmd160_r1 = [
    0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 7, 4, 13, 1, 10, 6,
    15, 3, 12, 0, 9, 5, 2, 14, 11, 8, 3, 10, 14, 4, 9, 15, 8, 1, 2, 7, 0, 6, 13,
    11, 5, 12, 1, 9, 11, 10, 0, 8, 12, 4, 13, 3, 7, 15, 14, 5, 6, 2, 4, 0, 5, 9,
    7, 12, 2, 10, 14, 1, 3, 8, 11, 6, 15, 13,
];
let rmd160_r2 = [
    5, 14, 7, 0, 9, 2, 11, 4, 13, 6, 15, 8, 1, 10, 3, 12, 6, 11, 3, 7, 0, 13, 5,
    10, 14, 15, 8, 12, 4, 9, 1, 2, 15, 5, 1, 3, 7, 14, 6, 9, 11, 8, 12, 2, 10,
    0, 4, 13, 8, 6, 4, 1, 3, 11, 15, 0, 5, 12, 2, 13, 9, 7, 10, 14, 12, 15, 10,
    4, 1, 5, 8, 7, 6, 2, 13, 14, 0, 3, 9, 11,
];
let rmd160_s1 = [
    11, 14, 15, 12, 5, 8, 7, 9, 11, 13, 14, 15, 6, 7, 9, 8, 7, 6, 8, 13, 11, 9,
    7, 15, 7, 12, 15, 9, 11, 7, 13, 12, 11, 13, 6, 7, 14, 9, 13, 15, 14, 8, 13,
    6, 5, 12, 7, 5, 11, 12, 14, 15, 14, 15, 9, 8, 9, 14, 5, 6, 8, 6, 5, 12, 9,
    15, 5, 11, 6, 8, 13, 12, 5, 12, 13, 14, 11, 8, 5, 6,
];
let rmd160_s2 = [
    8, 9, 9, 11, 13, 15, 15, 5, 7, 7, 8, 11, 14, 14, 12, 6, 9, 13, 15, 7, 12, 8,
    9, 11, 7, 7, 12, 7, 6, 15, 13, 11, 9, 7, 15, 11, 8, 6, 6, 14, 12, 13, 5, 14,
    13, 13, 7, 5, 15, 5, 8, 11, 14, 14, 6, 14, 6, 9, 12, 9, 12, 5, 15, 8, 8, 5,
    12, 9, 12, 5, 14, 6, 8, 13, 6, 5, 15, 13, 11, 11,
];

/*
 * Add integers, wrapping at 2^32. This uses 16-bit operations internally
 * to work around bugs in some JS interpreters.
 */
function safe_add(x, y) {
    let lsw = (x & 0xffff) + (y & 0xffff);
    let msw = (x >> 16) + (y >> 16) + (lsw >> 16);
    return (msw << 16) | (lsw & 0xffff);
}

/*
 * Bitwise rotate a 32-bit number to the left.
 */
function bit_rol(num, cnt) {
    return (num << cnt) | (num >>> (32 - cnt));
}
///////////////////////////////////////////////////////////////////////////////
//                               joegame added                               //
///////////////////////////////////////////////////////////////////////////////

/*
 * Gives a hash
 */
export function xyhash(x: number, y: number, seed = "") {
    const xy = `${x}--${y}` + seed;
    return hex_rmd160(xy);
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
