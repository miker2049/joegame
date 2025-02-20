// -*- lsp-enabled-clients: (deno-ls); -*-
import { readFileSync, writeFileSync } from "node:fs";
import {
    describe,
    it,
    beforeEach,
    beforeAll as before,
} from "https://deno.land/std@0.182.0/testing/bdd.ts";
import { expect } from "https://cdn.skypack.dev/chai@4.3.4?dts";

import TiledRawJSON from "../../joegamelib/src/types/TiledRawJson.d.ts";
import {
    addObjectTiles,
    createPackSection,
    saturateObjects,
} from "./saturator.ts";
import { embedTilesetsOffline } from "./embedTilesetOffline.ts";
import { isValidTilemap } from "./utils.ts";
import data from "../../../assets/data.json" assert { type: "json" };
import { TiledMap } from "./TiledMap.ts";

describe("Embedding tilesets in tilemaps", function () {
    it("Runs sucessfully and creates a valid tilemap", async function () {
        const tm: TiledRawJSON = JSON.parse(
            readFileSync("../../assets/maps/testmap_embed.json", "utf-8")
        );
        await embedTilesetsOffline(tm);

        // writeFileSync("../../assets/maps/ttt.json", JSON.stringify(tm));

        const valid = await isValidTilemap(tm);
        expect(valid, "The tilemap is valid").to.be.true;
    });
});

describe("Adding tiles for defined objects", function () {
    it("runs", async function () {
        let tm: TiledRawJSON = JSON.parse(
            readFileSync("../../assets/maps/testmap_embed.json", "utf-8")
        );
        const tmap = new TiledMap(tm);
        await embedTilesetsOffline(tm);
        const newg = tmap.addTileset(
            "gg",
            "../images/" +
                data.mapobject["dead-tree"].tile_config.texture +
                ".png",
            {
                margin: 1,
                spacing: 2,
                tileheight: 16,
                tilewidth: 16,
                tilecount: 1960,
                imageheight: 1764,
                imagewidth: 360,
                columns: 20,
            }
        );
        addObjectTiles(
            { x: 10 * 16, y: 10 * 16, ...data.mapobject["dead-tree"] },
            tm,
            newg!,
            "obj"
        );
        addObjectTiles(
            { x: 5 * 16, y: 10 * 16, ...data.mapobject["dead-tree"] },
            tm,
            newg!,
            "obj"
        );
        addObjectTiles(
            { x: 10 * 16, y: 5 * 16, ...data.mapobject["dead-tree"] },
            tm,
            newg!,
            "obj"
        );

        writeFileSync("../../assets/maps/ttt.json", JSON.stringify(tm));

        const valid = await isValidTilemap(tm);
        expect(valid, "The tilemap is valid").to.be.true;
    });
});

describe("saturateObjects", function () {
    it("runs without error and makes a valid tilemap", async function () {
        let tm: TiledRawJSON = JSON.parse(
            readFileSync("../../assets/maps/testmap_embed.json", "utf-8")
        );

        tm = await embedTilesetsOffline(tm);
        tm = await saturateObjects(tm);
        writeFileSync("../../assets/maps/sat.json", JSON.stringify(tm));

        const valid = await isValidTilemap(tm);
        expect(valid, "The tilemap is valid").to.be.true;
    });
});

describe("createPackSection", function () {
    let tm: TiledRawJSON;
    before(async function () {
        tm = JSON.parse(
            readFileSync("../../assets/maps/testmap_embed.json", "utf-8")
        );
        tm = await embedTilesetsOffline(tm);
    });
    it("creates a pack file with image dependencies for the tilesets", async function () {
        const t = await createPackSection(tm);
        expect(
            t.main.files.map((item) => item.key.replace(/.png$/, ""))
        ).to.include("11_Camping_16x16_nograss");
    });
    it("includes assets needed for tilemap objects", async function () {
        const t = await createPackSection(tm);

        expect(
            t.main.files.map((item) => item.key.replace(/.png$/, ""))
        ).to.include("browserquestextrude");
    });
});

describe("full saturation", function () {
    it("Makes a valid map", async function () {
        let tm: TiledRawJSON = JSON.parse(
            readFileSync("../../assets/maps/testmap_embed.json", "utf-8")
        );
        tm = await embedTilesetsOffline(tm);
        tm = await saturateObjects(tm);
        const ft = { pack: createPackSection(tm), ...tm };
        const valid = await isValidTilemap(ft);
        expect(valid, "The tilemap is valid").to.be.true;
        expect(ft.pack).to.not.be.undefined;
        writeFileSync("../../assets/maps/full.json", JSON.stringify(ft));
    });
});
