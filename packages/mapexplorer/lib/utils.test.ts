import { expect, test } from "vitest";
import { getRectTiles, ObjectPool } from "./utils";
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
