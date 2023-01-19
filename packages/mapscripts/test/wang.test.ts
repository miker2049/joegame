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
import { createCanvas } from "canvas";
import { loadMap } from "joegamelib/src/loadMap";
import dom from "jsdom-global"
dom()
import { tiledMapFromFile } from "../src/utils-node";

test("mock phaser canvas", async (t) => {
    const cnv = createCanvas(800, 600)
    const mapp = await loadMap({
        mapPath: "../../assets/maps/desert/desert-stamps2.json",
        gameConfigOverrides: {
            type: Phaser.HEADLESS
        },
    });

    t.assert(mapp[0] !== undefined);
    t.end();
});

test("small text-input wang output", async (t) => {
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
    // const wg = new WorldGen();
    const multed = scaleGrid(grid2, 2);
    if (!multed) throw Error("Issue scaling input");
    const stampsfile = await tiledMapFromFile(
        "../../assets/maps/desert/desert-stamps2.json"
    );
    const outmap = makeWangMapFrom2DArr(multed, stampsfile, "cliffs");
    console.log(multed);
    await writeFile(
        "../../assets/maps/desert/mini.json",
        JSON.stringify(outmap)
    );
    t.end();
});
