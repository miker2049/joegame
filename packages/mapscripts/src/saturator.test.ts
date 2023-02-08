import { readFileSync, writeFileSync } from "fs";
import { expect } from "chai";
import { describe, it, beforeEach } from "mocha";
import TiledRawJSON from "joegamelib/src/types/TiledRawJson";
import { addObjectTiles, embedTilesetsOffline } from "./saturator";
import { isValidTilemap } from "./utils-node";
import data from "assets/data.json";
import { TiledMap } from "./TiledMap";

describe("Embedding tilesets in tilemaps", function () {
    it("Runs sucessfully and creates a valid tilemap", async function () {
        this.timeout(-1);
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
        this.timeout(-1);
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
            }
        );
        await addObjectTiles(
            { x: 10 * 16, y: 10 * 16, ...data.mapobject["dead-tree"] },
            tm,
            newg!
        );
        await addObjectTiles(
            { x: 5 * 16, y: 10 * 16, ...data.mapobject["dead-tree"] },
            tm,
            newg!
        );
        await addObjectTiles(
            { x: 10 * 16, y: 5 * 16, ...data.mapobject["dead-tree"] },
            tm,
            newg!
        );

        writeFileSync("../../assets/maps/ttt.json", JSON.stringify(tm));

        const valid = await isValidTilemap(tm);
        expect(valid, "The tilemap is valid").to.be.true;
    });
});
