import test from "tape";
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
import { tiledMapFromFile } from "../src/utils-node";

test("small text-input wang output", async (t) => {
    const grid2 = DataGrid.fromGrid([
        [0, 0, 0, 0, 0],
        [0, 1, 1, 1, 0],
        [0, 1, 1, 0, 0],
        [0, 1, 1, 1, 0],
        [0, 0, 0, 0, 0],
    ]);
    const grid4 = DataGrid.fromGrid([
        [0, 0, 0],
        [0, 1, 0],
        [0, 0, 0],
    ]);
    const multed = scaleGrid(grid2, 2);
    if (!multed) throw Error("Issue scaling input");

    const stampsfile = await tiledMapFromFile(
        "../../assets/maps/desert/desert-stamps2.json"
    );
    const outmap = makeWangMapFrom2DArr(multed, stampsfile, "dirt");
    await writeFile(
        "../../assets/maps/desert/mini.json",
        JSON.stringify(outmap)
    );
    t.end();
});
