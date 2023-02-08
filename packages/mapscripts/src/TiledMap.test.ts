import { describe, it } from "mocha";
import { expect } from "chai";
import { TiledMap } from "../src/TiledMap";
import fs from "fs";

describe("shares the same data between config and grid 'view'", () => {
    const template = JSON.parse(fs.readFileSync("./src/empty.json", "utf8"));
    let tm = TiledMap.createEmpty(3, 3, template);
    tm.addEmptyLayer("test");
    tm.lg[0].setVal(2, 2, 420);
    const conf = tm.getConf();
    expect(conf.layers[0].data[8]).to.equal(420);
    tm.addEmptyLayer("test2");
    tm.lg[1].setVal(0, 0, 69);
    expect(conf.layers[0].data[8]).to.equal(420);
    expect(conf.layers[1].data[0]).to.equal(69);
    expect(tm.lg[1].at(0, 0)).to.equal(69);
    expect(tm.lg[0].at(2, 2)).to.equal(420);
});

describe("Tiledmap's allObjects helper function.", function () {
    it("correctly grabs all the objects in all the layers", function () {
        const template = JSON.parse(
            fs.readFileSync("../../assets/maps/testmap.json", "utf8")
        );
        const m = new TiledMap(template);
        console.log(m.allObjects().length, "HAIGH");
    });
});
