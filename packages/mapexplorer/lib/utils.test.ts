import { expect, test } from "vitest";
import {
    getRectTiles,
    ObjectPool,
    cantor,
    invertCantor,
    hashint,
    invertHashint,
    bitMaskUnion,
    bitMaskIntersection,
    subtractIntersection,
    gridEmpty,
} from "./utils";
import { Sprite } from "pixi.js";

test("getRectTiles works", () => {
    const twobytwo = getRectTiles({ x: 0, y: 0, w: 2, h: 2 });
    const twobytwooffset = getRectTiles({ x: 10, y: 10, w: 2, h: 2 });
    expect(twobytwo).toEqual([
        [0, 0],
        [1, 0],
        [0, 1],
        [1, 1],
    ]);
    expect(twobytwooffset).toEqual([
        [10, 10],
        [11, 10],
        [10, 11],
        [11, 11],
    ]);
});

test("ObjectPool", () => {
    const pool = new ObjectPool(10, Sprite, []);
    expect(pool.get() instanceof Sprite).toBe(true);
});

test("cantor", () => {
    expect(invertCantor(cantor(420, 69))).toStrictEqual([420, 69]);
    expect(invertCantor(cantor(256 ** 2, 256 ** 2))).toStrictEqual([
        256 ** 2,
        256 ** 2,
    ]);
    expect(invertCantor(cantor(1024 ** 2, 1024 ** 2))).toStrictEqual([
        1024 ** 2,
        1024 ** 2,
    ]);
    expect(invertCantor(cantor(1024 ** 4, 1024 ** 4))).toStrictEqual([
        1024 ** 4,
        1024 ** 4,
    ]);
});

test("hashInt", () => {
    expect(invertHashint(hashint(420, 69, 911))).toStrictEqual([420, 69, 911]);
});

test("bitmask operations work", () => {
    const grid1 = [
        [1, 0, 1],
        [0, 1, 0],
        [1, 0, 1],
    ];
    const grid2 = [
        [1, 1, 0],
        [1, 1, 1],
        [0, 1, 0],
    ];

    const unionresult = [
        [1, 1, 1],
        [1, 1, 1],
        [1, 1, 1],
    ];

    expect(bitMaskUnion(grid1, grid2)).toEqual(unionresult);

    expect(bitMaskIntersection(grid1, grid2)).toEqual([
        [1, 0, 0],
        [0, 1, 0],
        [0, 0, 0],
    ]);

    expect(subtractIntersection(grid1, grid2)).toEqual([
        [0, 0, 1],
        [0, 0, 0],
        [1, 0, 1],
    ]);

    expect(
        gridEmpty([
            [0, 0, 0, 0],
            [0, 0, 0, 0],
            [0, 0, 0, 0],
            [0, 0, 0, 0],
        ]),
    ).toBe(true);

    expect(
        gridEmpty([
            [1, 0, 0, 0],
            [0, 0, 0, 0],
            [0, 0, 0, 0],
            [0, 0, 0, 0],
        ]),
    ).toBe(false);
});
