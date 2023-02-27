import { describe, it } from "mocha";
import { expect } from "chai";
import { TiledMap } from "../src/TiledMap";
import fs from "fs";
import { isValidTilemap } from "./utils-node";
import { writeFile } from "fs/promises";

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

describe("adding tileset to map", function () {
    it("adds it without issue", async function () {
        const template = JSON.parse(
            fs.readFileSync("../../assets/maps/testmap.json", "utf8")
        );
        const m = new TiledMap(template);
        m.addTileset("browserquest", "../images/browserquestextrude.png", {
            margin: 1,
            spacing: 2,
            tileheight: 16,
            tilewidth: 16,
            imageheight: 1764,
            imagewidth: 360,
            tilecount: 1960,
            columns: 20,
        });
        const valid = await isValidTilemap(m.getConf());
        expect(valid).to.be.true;
        m.addTileset("19_Hospital_16x16", "../images/19_Hospital_16x16.png", {
            margin: 0,
            spacing: 0,
            tileheight: 16,
            tilewidth: 16,
            imageheight: 1760,
            imagewidth: 256,
            tilecount: 1760,
            columns: 16,
        });
        const valid2 = await isValidTilemap(m.getConf());
        expect(valid2).to.be.true;

        await writeFile(
            "../../assets/maps/tm2.json",
            JSON.stringify(m.getConf())
        );
    });
});

describe("adding object layer to map", function () {
    it("adds a valid objectlayer", async function () {
        const template = JSON.parse(
            fs.readFileSync("../../assets/maps/testmap.json", "utf8")
        );
        const m = new TiledMap(template);
        m.addObjectLayer("test-objects");

        const valid = await isValidTilemap(m.getConf());
        expect(valid).to.be.true;
    });
});
