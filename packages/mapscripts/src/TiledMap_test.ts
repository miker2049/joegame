// -*- lsp-enabled-clients: (deno-ls); -*-
import { describe, it } from "https://deno.land/std@0.182.0/testing/bdd.ts";
import { expect } from "https://cdn.skypack.dev/chai@4.3.4?dts";
import { TiledMap } from "./TiledMap.ts";
import { TiledMapCompressed } from "./TiledMapCompressed.ts";
import { isValidTilemap } from "./utils.ts";
import { ITileLayer } from "../../joegamelib/src/types/TiledRawJson.d.ts";

describe("shares the same data between config and grid 'view'", () => {
    const template = JSON.parse(Deno.readTextFileSync("./src/empty.json"));
    const tm = TiledMap.createEmpty(3, 3, template);
    tm.addEmptyLayer("test");
    tm.lg[0].setVal(2, 2, 420);
    const conf = tm.getConf();
    const lay1 = conf.layers[0] as ITileLayer;
    const lay2 = conf.layers[1] as ITileLayer;
    expect(lay1.data[8]).to.equal(420);
    tm.addEmptyLayer("test2");
    tm.lg[1].setVal(0, 0, 69);
    expect(lay1.data[8]).to.equal(420);
    expect((conf.layers[1] as ITileLayer).data[0]).to.equal(69);
    expect(tm.lg[1].at(0, 0)).to.equal(69);
    expect(tm.lg[0].at(2, 2)).to.equal(420);
});

describe("Tiledmap's allObjects helper function.", function () {
    it("correctly grabs all the objects in all the layers", function () {
        const template = JSON.parse(
            Deno.readTextFileSync("../../assets/maps/testmap.json")
        );
        const m = new TiledMap(template);
        console.log(m.allObjects().length, "HAIGH");
    });
});

describe("adding tileset to map", function () {
    it("adds it without issue", async function () {
        const template = JSON.parse(
            Deno.readTextFileSync("../../assets/maps/testmap.json")
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

        await Deno.writeTextFile(
            "../../assets/maps/tm2.json",
            JSON.stringify(m.getConf())
        );
    });
});

describe("adding object layer to map", function () {
    it("adds a valid objectlayer", async function () {
        const template = JSON.parse(
            Deno.readTextFileSync("../../assets/maps/testmap.json")
        );
        const m = new TiledMap(template);
        const lay = m.addObjectLayer("test-objects");

        const valid = await isValidTilemap(m.getConf());
        expect(valid).to.be.true;

        const obj = m.addObject("tree", 100, 100, lay);

        const valid2 = await isValidTilemap(m.getConf());

        expect(valid2).to.be.true;
    });
});

describe("TiledMapCompressed", function () {
    it("can make a valid map after compression", async function () {
        const template = JSON.parse(
            Deno.readTextFileSync("../../assets/maps/testmap.json")
        );
        const m = new TiledMapCompressed(template);
        m.compressLayers();
        const valid = await isValidTilemap(m.getConf());

        await Deno.writeTextFile(
            "../../assets/maps/compressed.json",
            JSON.stringify(m.getConf())
        );

        expect(valid).to.be.true;
    });
});
