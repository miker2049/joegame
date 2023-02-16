/**
 * Made by robots
 */
interface RGB {
    r: number;
    g: number;
    b: number;
    a?: number;
}

interface HSV {
    h: number;
    s: number;
    v: number;
}

//https://stackoverflow.com/a/727339
export function addColor(fg: RGB, bg: RGB) {
    const r: RGB = { r: 0, g: 0, b: 0, a: 0 };
    fg = {
        r: fg.r / 255,
        g: fg.r / 255,
        b: fg.r / 255,
        a: (fg.a || 255) / 255,
    };
    fg.a = (fg.a || 255) / 255;
    bg = {
        r: bg.r / 255,
        g: bg.r / 255,
        b: bg.r / 255,
        a: (bg.a || 255) / 255,
    };
    bg.a = (bg.a || 255) / 255;

    r.a = 1 - (1 - fg.a) * (1 - bg.a);
    if (r.a < 1.0e-6) return bg; // fully transparent -- r,g,b not important
    r.r = (fg.r * fg.a) / r.a + (bg.r * bg.a * (1 - fg.a)) / r.a;
    r.g = (fg.g * fg.a) / r.a + (bg.g * bg.a * (1 - fg.a)) / r.a;
    r.b = (fg.b * fg.a) / r.a + (bg.b * bg.a * (1 - fg.a)) / r.a;
    return { r: r.r * 255, g: r.g * 255, b: r.b * 255, a: r.a * 255 };
}

export function multiplyColor(fg: RGB, bg: RGB) {
    return {
        r: fg.r * bg.r,
        g: fg.g * bg.g,
        b: fg.b * bg.b,
        a: fg.a || 255,
    };
}
export function darken(c: RGB, l: number) {
    // return multiplyColor({ r: l, g: l, b: l, a: l }, c);
    const hsv = rgbToHsv(c);
    hsv.v -= l;
    return hsvToRgb(hsv);
    // return {
    //     r: c.r * (1 - l),
    //     g: c.g * (1 - l),
    //     b: c.b * (1 - l),
    //     a: c.a,
    // };
}
export function hexToRGB(s: string): RGB {
    const matches = s.match(/^#?(\w\w)(\w\w)(\w\w)/);
    if (matches)
        return {
            r: parseInt(matches[1], 16),
            g: parseInt(matches[2], 16),
            b: parseInt(matches[3], 16),
        };
    else
        return {
            r: 0,
            g: 0,
            b: 0,
        };
}
function hexVal(s: number) {
    return ("00" + Number(s).toString(16)).slice(-2);
}

export function rgbToHex(c: RGB) {
    return `#${hexVal(c.r)}${hexVal(c.g)}${hexVal(c.b)}${hexVal(c.a || 255)}`;
}

export function rgbToHsv(rgb: RGB): HSV {
    const r = rgb.r / 255;
    const g = rgb.g / 255;
    const b = rgb.b / 255;
    const max = Math.max(r, g, b);
    const min = Math.min(r, g, b);
    const delta = max - min;

    let h = 0;
    let s = 0;
    let v = max;

    if (delta !== 0) {
        s = delta / max;
        if (max === r) {
            h = (g - b) / delta + (g < b ? 6 : 0);
        } else if (max === g) {
            h = (b - r) / delta + 2;
        } else {
            h = (r - g) / delta + 4;
        }
        h /= 6;
    }

    return {
        h: h * 360,
        s: s * 100,
        v: v * 100,
    };
}

export function hsvToRgb(hsv: HSV): RGB {
    const h = hsv.h / 360;
    const s = hsv.s / 100;
    const v = hsv.v / 100;

    const i = Math.floor(h * 6);
    const f = h * 6 - i;
    const p = v * (1 - s);
    const q = v * (1 - f * s);
    const t = v * (1 - (1 - f) * s);

    let r = 0;
    let g = 0;
    let b = 0;

    switch (i % 6) {
        case 0:
            r = v;
            g = t;
            b = p;
            break;
        case 1:
            r = q;
            g = v;
            b = p;
            break;
        case 2:
            r = p;
            g = v;
            b = t;
            break;
        case 3:
            r = p;
            g = q;
            b = v;
            break;
        case 4:
            r = t;
            g = p;
            b = v;
            break;
        case 5:
            r = v;
            g = p;
            b = q;
            break;
    }

    return {
        r: Math.round(r * 255),
        g: Math.round(g * 255),
        b: Math.round(b * 255),
    };
}
