import { expect } from "chai";
import { describe, it, beforeEach } from "mocha";
import { DataGrid, scaledXY, scaleGrid, weightedChoose } from "./utils";
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

describe("weightedChoose", function () {
    it("gives a value", function () {
        const tt = ["a", "b", "c"];
        const tw = [0.3, 0.3, 0.9];
        const out = weightedChoose(tt, tw);
        console.log(out);
        expect(out).to.not.be.undefined;
    });
    it("gives a deterministic value when the rand value is given", function () {
        const tt = ["a", "b", "c"];
        const tw = [0.3, 0.3, 0.9];
        const out = weightedChoose(tt, tw, 0.5);
        const out2 = weightedChoose(tt, tw, 0.5);
        expect(out).to.equal(out2);
    });
    it("gives expected val when rand input is too big or small", function () {
        const tt = ["a", "b", "c"];
        const tw = [0.3, 0.3, 0.9];
        const one = weightedChoose(tt, tw, 1);

        expect(one, "val of one").to.not.be.undefined;
        const bignum = weightedChoose(tt, tw, 100);
        expect(bignum).to.not.be.undefined;
        const negativenum = weightedChoose(tt, tw, -100);
        expect(negativenum).to.not.be.undefined;
        const zero = weightedChoose(tt, tw, 0);
        expect(zero).to.not.be.undefined;
    });
});
