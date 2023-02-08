import { expect } from "chai";
import { describe, it, beforeEach } from "mocha";
import { DataGrid, scaledXY, scaleGrid } from "./utils";
import { tmpfile } from "./utils-node";

describe("scaledXY", function () {
    const g = DataGrid.fromGrid([
        [0, 0, 0],
        [0, 1, 0],
        [0, 0, 0],
    ]);

    it("retrieves values from a grid of the specified scale", function () {
        expect(scaledXY(g, 2, 2, 2)).to.equal(1);
        expect(scaledXY(g, 2, 3, 2)).to.equal(1);
        expect(scaledXY(g, 2, 2, 3)).to.equal(1);
        expect(scaledXY(g, 2, 3, 3)).to.equal(1);
        expect(scaledXY(g, 2, 4, 4)).to.equal(0);
    });
});

describe("tmpfile", function () {
    it("runs successfully", function () {
        const out = tmpfile();
        console.log(out);
    });
});
