import { TiledMap } from "../src/TiledMap";
import { writeFile } from "fs/promises";
import {
    collectSubArr,
    createEmptyTiledMap,
    DataGrid,
    makeWangMapFrom2DArr,
    multiplyGrids,
    scaleGrid,
} from "../src/utils";
import { createCanvas } from "canvas";
import { loadMap } from "joegamelib/src/loadMap";
import { tiledMapFromFile } from "../src/utils-node";
import { describe, it } from "mocha";
import { expect } from "chai";
const test = describe;

describe("small text-input wang output", async function () {
    const grid2 = DataGrid.fromGrid([
        [0, 0, 0, 0, 0, 0],
        [0, 1, 1, 1, 0, 0],
        [0, 1, 1, 0, 0, 0],
        [0, 1, 1, 1, 0, 0],
        [0, 0, 0, 0, 0, 0],
        [0, 0, 0, 0, 0, 0],
    ]);
    const grid4 = DataGrid.fromGrid([
        [0, 0, 0],
        [0, 1, 0],
        [0, 0, 0],
    ]);
    it("doubles predictably with scaleGrid", function () {
        const multed = scaleGrid(grid2, 2);
        expect(!multed);
        expect(multed.width).to.be.equal(
            grid2.width * 2,
            "The width is doubled"
        );
        expect(multed.height()).to.be.equal(
            grid2.height() * 2,
            "The height is doubled"
        );
    });
    // const wg = new WorldGen();
    it("can write a valid tilemap file", async function () {
        const multed = scaleGrid(grid2, 2);
        const stampsfile = await tiledMapFromFile(
            "../../assets/maps/desert-stamps2.json"
        );

        const outmap = makeWangMapFrom2DArr(multed, stampsfile, "cliffs");
        await writeFile("../../assets/maps/mini.json", JSON.stringify(outmap));
    });
});
