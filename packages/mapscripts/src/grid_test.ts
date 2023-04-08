// -*- lsp-enabled-clients: (deno-ls); -*-
import {
    addChunk,
    attachTileChunks,
    collectSubArr,
    DataGrid,
    encodeGrid,
    injectChunk,
    makeEmptyGrid,
    scaleGrid,
    StyleDir,
    unflat,
} from "./utils.ts";

import { describe, it } from "https://deno.land/std@0.182.0/testing/bdd.ts";
import { expect } from "https://cdn.skypack.dev/chai@4.3.4?dts";
const test = it;

const t = {
    equal: (v: unknown, vv: unknown, m = "") => expect(v, m).to.equal(vv),
    deepEqual: (v: unknown, vv: unknown, m = "") =>
        expect(v, m).to.deep.equal(vv),
};

describe("grid main", function () {
    test("encoding grid", () => {
        t.equal(
            encodeGrid(
                DataGrid.fromGrid([
                    [0, 0, 0],
                    [0, 0, 0],
                    [0, 0, 0],
                ]),
                1
            ),
            0
        );
        t.equal(
            encodeGrid(
                DataGrid.fromGrid([
                    [1, 0, 0],
                    [0, 1, 0],
                    [0, 0, 1],
                ]),
                1
            ),
            273
        );
        t.equal(
            encodeGrid(
                DataGrid.fromGrid([
                    [0, 0, 0],
                    [0, 1, 0],
                    [0, 1, 1],
                ]),
                1
            ),
            19
        );
        t.equal(
            encodeGrid(
                DataGrid.fromGrid([
                    [1, 0, 0],
                    [0, 0, 0],
                    [0, 0, 0],
                ]),
                1
            ),
            256
        );
        t.equal(
            encodeGrid(
                DataGrid.fromGrid([
                    [1, 0, 0],
                    [0, 0, 1],
                    [0, 0, 0],
                ]),
                1
            ),
            264,
            "two apart"
        );
    });

    test("collectSubArr predicatably collects subarrays", () => {
        const grid = DataGrid.fromGrid([
            [1, 2, 3, 4, 5, 6, 7, 8],
            [1, 2, 3, 4, 5, 6, 7, 8],
            [1, 2, 3, 4, 5, 6, 7, 8],
            [1, 2, 3, 4, 5, 6, 7, 8],
            [1, 2, 3, 4, 5, 6, 7, 8],
            [1, 2, 3, 4, 5, 6, 7, 8],
            [1, 2, 3, 4, 5, 6, 7, 8],
            [1, 2, 3, 4, 5, 6, 7, 8],
        ]);
        const collected = collectSubArr(4, 4, grid);
        t.equal(collected[0].at(1, 0), 2);
        t.equal(collected[1].at(1, 0), 6);
        t.equal(collected[2].at(1, 0), 2);
        t.equal(collected[3].at(1, 0), 6);
        t.equal(collected.length, 4);
    });

    test("makeEmptyGrid function", () => {
        t.deepEqual(
            makeEmptyGrid(3, 3, 0).getData(),
            [
                [0, 0, 0],
                [0, 0, 0],
                [0, 0, 0],
            ].flat()
        );
    });

    test("attachTileChunks will overlap grids", () => {
        const g = DataGrid.fromGrid([
            [0, 0],
            [0, 0],
        ]);
        const r = DataGrid.fromGrid([
            [1, 2],
            [3, 4],
        ]);
        const r1 = DataGrid.fromGrid([
            [0, 1, 2],
            [0, 3, 4],
        ]);
        const r2 = DataGrid.fromGrid([
            [0, 0, 0],
            [0, 2, 0],
        ]);
        t.deepEqual(attachTileChunks(g, r, 0, 0, 0).getData(), r.getData());
        t.deepEqual(attachTileChunks(g, r, 1, 0, 0).getData(), r1.getData());
        t.deepEqual(attachTileChunks(g, r2, 0, 0, 0).getData(), r2.getData());
    });

    test("fast injects", () => {
        const g = DataGrid.fromGrid([
            [0, 0],
            [0, 0],
        ]);
        const r = DataGrid.fromGrid([
            [1, 2],
            [3, 4],
        ]);
        const r1 = DataGrid.fromGrid([
            [0, 1, 2],
            [0, 3, 4],
        ]);
        const r2 = DataGrid.fromGrid([
            [0, 6],
            [2, 9],
        ]);
        const rr = DataGrid.fromGrid([
            [0, 0, 6],
            [0, 2, 9],
        ]);
        const rr2 = DataGrid.fromGrid([
            [0, 1, 0],
            [0, 3, 2],
        ]);
        t.deepEqual(injectChunk(g, r, 0, 0).getData(), r.getData());
        t.deepEqual(injectChunk(r1.clone(), r2, 1, 0).getData(), rr.getData());
        t.deepEqual(injectChunk(r1.clone(), r2, 2, 0).getData(), rr2.getData());
    });

    test("attachTileChunks will grow according to offsets", () => {
        const grid1 = DataGrid.fromGrid([
            [0, 0, 0, 0, 0],
            [0, 0, 0, 0, 0],
            [0, 0, 0, 0, 0],
            [0, 0, 0, 0, 0],
            [0, 0, 0, 0, 0],
        ]);
        const grid2 = DataGrid.fromGrid([
            [1, 1, 1],
            [1, 1, 1],
            [1, 1, 1],
            [1, 1, 1],
            [1, 1, 1],
        ]);
        const res = DataGrid.fromGrid([
            [0, 0, 0, 0, 0, 0, 0, 0, 0],
            [0, 0, 0, 0, 0, 0, 0, 0, 0],
            [0, 0, 0, 0, 0, 0, 0, 0, 0],
            [0, 0, 0, 0, 0, 0, 0, 0, 0],
            [0, 0, 0, 0, 0, 0, 0, 0, 0],
            [0, 0, 0, 0, 0, 0, 0, 0, 0],
            [0, 0, 0, 0, 0, 0, 1, 1, 1],
            [0, 0, 0, 0, 0, 0, 1, 1, 1],
            [0, 0, 0, 0, 0, 0, 1, 1, 1],
            [0, 0, 0, 0, 0, 0, 1, 1, 1],
            [0, 0, 0, 0, 0, 0, 1, 1, 1],
        ]);

        const res2 = DataGrid.fromGrid([
            [1, 1, 1, 0, 0, 0, 0, 0, 0, 1, 1, 1],
            [1, 1, 1, 0, 0, 0, 0, 0, 0, 1, 1, 1],
            [1, 1, 1, 0, 0, 0, 0, 0, 0, 1, 1, 1],
            [1, 1, 1, 0, 0, 0, 0, 0, 0, 1, 1, 1],
            [1, 1, 1, 0, 0, 0, 0, 0, 0, 1, 1, 1],
        ]);
        const res3 = DataGrid.fromGrid([
            [1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0],
            [1, 1, 1, 0, 0, 0, 0, 0, 0, 1, 1, 1],
            [1, 1, 1, 0, 0, 0, 0, 0, 0, 1, 1, 1],
            [1, 1, 1, 0, 0, 0, 0, 0, 0, 1, 1, 1],
            [1, 1, 1, 0, 0, 0, 0, 0, 0, 1, 1, 1],
            [0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1],
        ]);

        const out = attachTileChunks(grid1.clone(), grid2.clone(), 6, 6, 0);
        t.deepEqual(out.getData(), res.getData());
        const out2 = attachTileChunks(grid2.clone(), grid2.clone(), 9, 0, 0);
        t.deepEqual(out2.getData(), res2.getData());
        t.deepEqual(
            attachTileChunks(grid2.clone(), grid2.clone(), 9, 1, 0).getData(),
            res3.getData()
        );
    });

    test("attachTileChunks will grow according to negative offsets", () => {
        const grid1 = DataGrid.fromGrid([
            [0, 0, 0, 0, 0],
            [0, 0, 0, 0, 0],
            [0, 0, 0, 0, 0],
            [0, 0, 0, 0, 0],
            [0, 0, 0, 0, 0],
        ]);
        const grid2 = DataGrid.fromGrid([
            [1, 1, 1],
            [1, 1, 1],
            [1, 1, 1],
            [1, 1, 1],
            [1, 1, 1],
        ]);
        const res = DataGrid.fromGrid([
            [1, 1, 1, 0, 0],
            [1, 1, 1, 0, 0],
            [1, 1, 1, 0, 0],
            [1, 1, 1, 0, 0],
            [1, 1, 1, 0, 0],
            [0, 0, 0, 0, 0],
            [0, 0, 0, 0, 0],
            [0, 0, 0, 0, 0],
            [0, 0, 0, 0, 0],
            [0, 0, 0, 0, 0],
        ]);
        const res2 = DataGrid.fromGrid([
            [1, 1, 1, 0, 0, 0, 0, 0],
            [1, 1, 1, 0, 0, 0, 0, 0],
            [1, 1, 1, 0, 0, 0, 0, 0],
            [1, 1, 1, 0, 0, 0, 0, 0],
            [1, 1, 1, 0, 0, 0, 0, 0],
        ]);
        const res3 = DataGrid.fromGrid([
            [1, 1, 1, 0, 0, 0, 0, 0],
            [1, 1, 1, 0, 0, 0, 0, 0],
            [1, 1, 1, 0, 0, 0, 0, 0],
            [1, 1, 1, 0, 0, 0, 0, 0],
            [1, 1, 1, 0, 0, 0, 0, 0],
            [0, 0, 0, 0, 0, 0, 0, 0],
            [0, 0, 0, 0, 0, 0, 0, 0],
            [0, 0, 0, 0, 0, 0, 0, 0],
            [0, 0, 0, 0, 0, 0, 0, 0],
            [0, 0, 0, 0, 0, 0, 0, 0],
        ]);
        const res4 = DataGrid.fromGrid([
            [1, 1, 1, 0, 0, 0, 0],
            [1, 1, 1, 0, 0, 0, 0],
            [1, 1, 1, 0, 0, 0, 0],
            [1, 1, 1, 0, 0, 0, 0],
            [1, 1, 1, 0, 0, 0, 0],
        ]);
        const grid3 = DataGrid.fromGrid([
            [1, 1, 1],
            [1, 1, 1],
        ]);
        const grid4 = DataGrid.fromGrid([
            [2, 2],
            [2, 2],
        ]);
        const res5 = DataGrid.fromGrid([
            [0, 2, 2],
            [0, 2, 2],
            [1, 1, 1],
            [1, 1, 1],
        ]);
        const out = addChunk(grid1.clone(), grid2.clone(), 0, -5, 0);
        const out2 = addChunk(grid1.clone(), grid2.clone(), -3, 0, 0);
        const out3 = addChunk(grid1.clone(), grid2.clone(), -3, -5, 0);
        const out4 = addChunk(grid1.clone(), grid2.clone(), -2, 0, 0);
        const out5 = addChunk(grid3.clone(), grid4.clone(), 1, -2, 0);
        t.deepEqual(out.getData(), res.getData());
        t.deepEqual(out2.getData(), res2.getData());
        t.deepEqual(out3.getData(), res3.getData());
        t.deepEqual(out4.getData(), res4.getData());
        t.deepEqual(out5.getData(), res5.getData());
    });

    test("dynamic add chunks", () => {
        const g = DataGrid.fromGrid([
            [0, 0],
            [0, 0],
        ]);
        const r = DataGrid.fromGrid([
            [1, 2],
            [3, 4],
        ]);
        const r1 = DataGrid.fromGrid([
            [0, 1, 2],
            [0, 3, 4],
        ]);
        const r2 = DataGrid.fromGrid([
            [0, 6],
            [2, 9],
        ]);
        const rr = DataGrid.fromGrid([
            [0, 0, 6],
            [0, 2, 9],
        ]);
        const rr2 = DataGrid.fromGrid([
            [0, 1, 0],
            [0, 3, 2],
        ]);
        t.deepEqual(addChunk(g, r, 0, 0, undefined).getData(), r.getData());
        t.deepEqual(
            addChunk(r1.clone(), r2, 1, 0, undefined).getData(),
            rr.getData()
        );
        const grid1 = DataGrid.fromGrid([
            [0, 0, 0, 0, 0],
            [0, 0, 0, 0, 0],
            [0, 0, 0, 0, 0],
            [0, 0, 0, 0, 0],
            [0, 0, 0, 0, 0],
        ]);
        const grid2 = DataGrid.fromGrid([
            [1, 1, 1],
            [1, 1, 1],
            [1, 1, 1],
            [1, 1, 1],
            [1, 1, 1],
        ]);

        const res = DataGrid.fromGrid([
            [0, 0, 0, 0, 0, 0, 0, 0, 0],
            [0, 0, 0, 0, 0, 0, 0, 0, 0],
            [0, 0, 0, 0, 0, 0, 0, 0, 0],
            [0, 0, 0, 0, 0, 0, 0, 0, 0],
            [0, 0, 0, 0, 0, 0, 0, 0, 0],
            [0, 0, 0, 0, 0, 0, 0, 0, 0],
            [0, 0, 0, 0, 0, 0, 1, 1, 1],
            [0, 0, 0, 0, 0, 0, 1, 1, 1],
            [0, 0, 0, 0, 0, 0, 1, 1, 1],
            [0, 0, 0, 0, 0, 0, 1, 1, 1],
            [0, 0, 0, 0, 0, 0, 1, 1, 1],
        ]);

        const res2 = DataGrid.fromGrid([
            [1, 1, 1, 0, 0, 0, 0, 0, 0, 1, 1, 1],
            [1, 1, 1, 0, 0, 0, 0, 0, 0, 1, 1, 1],
            [1, 1, 1, 0, 0, 0, 0, 0, 0, 1, 1, 1],
            [1, 1, 1, 0, 0, 0, 0, 0, 0, 1, 1, 1],
            [1, 1, 1, 0, 0, 0, 0, 0, 0, 1, 1, 1],
        ]);
        const res3 = DataGrid.fromGrid([
            [1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0],
            [1, 1, 1, 0, 0, 0, 0, 0, 0, 1, 1, 1],
            [1, 1, 1, 0, 0, 0, 0, 0, 0, 1, 1, 1],
            [1, 1, 1, 0, 0, 0, 0, 0, 0, 1, 1, 1],
            [1, 1, 1, 0, 0, 0, 0, 0, 0, 1, 1, 1],
            [0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1],
        ]);

        const out = addChunk(grid1, grid2, 6, 6, 0);
        t.deepEqual(out.getData(), res.getData());
        t.deepEqual(
            addChunk(grid2.clone(), grid2, 9, 0, 0).getData(),
            res2.getData()
        );
        t.deepEqual(
            addChunk(grid2.clone(), grid2, 9, 1, 0).getData(),
            res3.getData()
        );
    });

    test("pad grid", () => {
        const r = DataGrid.fromGrid([
            [1, 2],
            [3, 4],
        ]);
        const lres = DataGrid.fromGrid([
            [0, 1, 2],
            [0, 3, 4],
        ]);
        const rres = DataGrid.fromGrid([
            [1, 2, 0],
            [3, 4, 0],
        ]);
        const tres = DataGrid.fromGrid([
            [0, 0],
            [1, 2],
            [3, 4],
        ]);
        const bres = DataGrid.fromGrid([
            [1, 2],
            [3, 4],
            [0, 0],
        ]);

        const padL = r.clone();
        const padR = r.clone();
        const padT = r.clone();
        const padB = r.clone();
        padL.pad(1, 0, StyleDir.left);
        padR.pad(1, 0, StyleDir.right);
        padT.pad(1, 0, StyleDir.top);
        padB.pad(1, 0, StyleDir.bottom);
        t.deepEqual(padR.getData(), rres.getData(), "right pad");
        t.deepEqual(padL.getData(), lres.getData(), "left pad");
        t.deepEqual(padT.getData(), tres.getData(), "top pad");
        t.deepEqual(padB.getData(), bres.getData(), " bottom pad");
    });

    test("scale grid", () => {
        const grid2 = DataGrid.fromGrid([
            [0, 0, 0, 0, 0, 0],
            [0, 1, 1, 1, 0, 0],
            [0, 1, 1, 0, 0, 0],
            [0, 1, 1, 1, 0, 0],
            [0, 0, 0, 0, 1, 0],
            [0, 0, 0, 0, 0, 0],
        ]);
        const m15 = scaleGrid(grid2, 1.5);
        const m05 = scaleGrid(grid2, 0.5);
        const m2 = scaleGrid(grid2, 2);
        t.equal(m2.width, 12);
        t.equal(m05.width, 3);
        t.equal(m15.width, 9);
    });
});
