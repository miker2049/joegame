import { expect } from "chai";
import { describe, it, beforeEach } from "mocha";
import {
    cullCoordinates,
    DataGrid,
    getDBRows,
    getObjectStep,
    getStepBoxEdge,
    scaledXY,
    scaleGrid,
    TileStacks,
    weightedChoose,
} from "./utils";
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

describe("TileStacks", function () {
    it("runs without error", function () {
        const ts = new TileStacks(25, 25);
        ts.push(5, 5, 420);
        ts.push(5, 5, 69);
        expect(ts.at(5, 5)).to.deep.equal([420, 69]);
        expect(ts.at(23, 11)).to.be.undefined;
        expect(ts.at(230, 110)).to.be.undefined;
    });
    it("The split method splits the stack based on a callback", function () {
        const ts = new TileStacks(5, 5);
        ts.push(0, 0, 420);
        ts.push(0, 1, 69);
        ts.push(0, 2, 68);
        ts.push(4, 2, 64);
        ts.push(4, 3, 3);
        const [a, b] = ts.split((v) => !Boolean(v % 2));
        expect(a.at(0, 0)).to.deep.equal([420]);
        expect(b.at(0, 0)).to.be.undefined;
        expect(b.at(0, 1)).to.deep.equal([69]);
        expect(a.at(0, 1)).to.be.undefined;
    });

    it("addChunk works as expected", function () {
        const ts = new TileStacks(25, 25);
        ts.addChunk(
            DataGrid.fromGrid([
                [5, 4, 3],
                [5, 4, 3],
                [5, 4, 3],
            ]),
            3,
            4
        );
        ts.addChunk(
            DataGrid.fromGrid([
                [5, 4, 3],
                [5, 4, 3],
                [5, 4, 3],
            ]),
            4,
            4
        );
        expect(ts.at(3, 4)).to.deep.equal([5]);
        expect(ts.at(4, 4)).to.deep.equal([4, 5]);
        expect(ts.at(5, 4)).to.deep.equal([3, 4]);
        expect(ts.at(6, 4)).to.deep.equal([3]);
    });
});

describe("Edge boxes", function () {
    it("getStepBoxEdge gives the right boxes", function () {
        const rightFor1 = [
            [-1, -1],
            [0, -1],
            [1, -1],
            [-1, 0],
            [1, 0],
            [-1, 1],
            [0, 1],
            [1, 1],
        ];
        const rightFor2 = [
            [-2, -2],
            [-1, -2],
            [0, -2],
            [1, -2],
            [2, -2],
            [-2, 2],
            [-1, 2],
            [0, 2],
            [1, 2],
            [2, 2],
            [-2, -1],
            [-2, 0],
            [-2, 1],
            [2, -1],
            [2, 0],
            [2, 1],
        ];
        expect(new Set(getStepBoxEdge(2)).entries()).to.deep.equal(
            new Set(rightFor2).entries()
        );
        expect(new Set(getStepBoxEdge(1)).entries()).to.deep.equal(
            new Set(rightFor1).entries()
        );
    });
    it("getObjectStep gives the right group/step with an idx and saturation number", function () {
        expect(getObjectStep(4, 1)).to.equal(0);
        expect(getObjectStep(4, 5)).to.equal(1);
        expect(getObjectStep(4, 37)).to.equal(2);
        expect(getObjectStep(4, 68)).to.equal(3);
        expect(getObjectStep(4, 104)).to.equal(4);
    });
});

describe.skip("db utils", function () {
    it("getDBRows is functional and returns useful rows", async function () {
        console.log(await getDBRows("jdb.db", "tweets", 500));
    });
});

describe("cullCoordinates", () => {
    it("should return an empty array when passed an empty array", () => {
        const result = cullCoordinates([], 10);
        expect(result).to.be.an("array").that.is.empty;
    });

    it("should not remove any coordinates when they are all far apart", () => {
        const coordinates = [
            { x: 0, y: 0, priority: 1 },
            { x: 20, y: 20, priority: 2 },
            { x: -40, y: -10, priority: 3 },
        ];

        const result = cullCoordinates(coordinates, 10);

        expect(result).to.have.lengthOf(coordinates.length);
        expect(result).to.deep.include.members(coordinates);
    });

    it("should remove one coordinate when two are near enough", () => {
        const coordinates = [
            { x: 0, y: 0, priority: 1 },
            { x: 5, y: 5, priority: 2 },
            { x: 20, y: 20, priority: 3 },
        ];

        const result = cullCoordinates(coordinates, 10);

        expect(result).to.have.lengthOf(coordinates.length - 1);
        expect(result).to.deep.include.members([
            { x: 5, y: 5, priority: 2 },
            { x: 20, y: 20, priority: 3 },
        ]);
    });

    it("should remove the coordinate with lower priority", () => {
        const coordinates = [
            { x: 0, y: 0, priority: 2 },
            { x: 5, y: 5, priority: 1 },
            { x: 20, y: 20, priority: 3 },
        ];

        const result = cullCoordinates(coordinates, 10);

        expect(result).to.have.lengthOf(coordinates.length - 1);
        expect(result).to.deep.include.members([
            { x: 0, y: 0, priority: 2 },
            { x: 20, y: 20, priority: 3 },
        ]);
    });

    it("should remove all but one coordinate when they are all too close", () => {
        const coordinates = [
            { x: 0, y: 0, priority: 1 },
            { x: 5, y: 5, priority: 2 },
            { x: 8, y: 8, priority: 3 },
        ];

        const result = cullCoordinates(coordinates, 10);

        expect(result).to.have.lengthOf(1);
        expect(result[0]).to.deep.equal({ x: 8, y: 8, priority: 3 });
    });

    it("should still remove coordinates when all priorities are the same", () => {
        const coordinates = [
            { x: 0, y: 0, priority: 1 },
            { x: 10, y: 10, priority: 1 },
            { x: 20, y: 20, priority: 1 },
        ];

        const result = cullCoordinates(coordinates, 15);

        expect(result).to.have.lengthOf(1);
        expect(result).to.deep.include.members([{ x: 20, y: 20, priority: 1 }]);
    });

    it("should remove two coordinates when three are near enough", () => {
        const coordinates = [
            { x: 0, y: 0, priority: 1 },
            { x: 8, y: 8, priority: 2 },
            { x: 15, y: 15, priority: 3 },
            { x: 22, y: 22, priority: 4 },
        ];

        const result = cullCoordinates(coordinates, 10);

        expect(result).to.have.lengthOf(2);
        expect(result).to.deep.include.members([
            { x: 0, y: 0, priority: 1 },
            { x: 22, y: 22, priority: 4 },
        ]);
    });

    it("should remove all but one coordinate when they are all too close", () => {
        const coordinates = [
            { x: 0, y: 0, priority: 1 },
            { x: 2, y: 2, priority: 2 },
            { x: 4, y: 4, priority: 3 },
            { x: 6, y: 6, priority: 4 },
            { x: 8, y: 8, priority: 5 },
            { x: 10, y: 10, priority: 6 },
            { x: 12, y: 12, priority: 7 },
            { x: 14, y: 14, priority: 8 },
            { x: 16, y: 16, priority: 9 },
        ];

        const result = cullCoordinates(coordinates, 3);
        expect(result).to.have.lengthOf(1);
        expect(result).to.deep.include.members([{ x: 16, y: 16, priority: 9 }]);
    });

    it("should not remove any coordinates when they are all far apart even with float point number", () => {
        const coordinates = [
            { x: 0, y: 0, priority: 1 },
            { x: 20.5, y: 20.5, priority: 2 },
            { x: -40.25, y: -5.25, priority: 3 },
        ];

        const result = cullCoordinates(coordinates, 10);

        expect(result).to.have.lengthOf(coordinates.length);
        expect(result).to.deep.include.members(coordinates);
    });
});
