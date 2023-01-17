import { readFileSync, writeFileSync } from "fs";
import { readFile, writeFile } from "fs/promises";
import TiledRawJSON from "joegamelib/src/types/TiledRawJson";
import { parseCSVRowsToGameData } from "joegamelib/src/utils/parseCSVRowsToGameData";
import { TiledMap } from "./TiledMap";
import path from "path";
import jimp from "jimp";

export async function readTiledFile(p: string): Promise<TiledRawJSON> {
    const fi = await readFile(p, "utf-8");
    return JSON.parse(fi);
}

export function dumpCSVData(p: string, o: string) {
    const fi = readFileSync(p, "utf-8");
    const data = parseCSVRowsToGameData(fi);
    const serialized = JSON.stringify({
        ...data,
        spritesheet: Object.fromEntries(data.spritesheet.entries()),
        character: Object.fromEntries(data.character.entries()),
        image: Object.fromEntries(data.image.entries()),
        platform: Object.fromEntries(data.platform.entries()),
        mapobject: Object.fromEntries(data.mapobject.entries()),
        sound: Object.fromEntries(data.sound.entries()),
        animatedTiles: Object.fromEntries(data.animatedTiles.entries()),
        // convoManifest: Object.fromEntries(data.convoManifest.entries()),
        // htmlImage: Object.fromEntries(data.htmlImage.entries()),
    });
    writeFileSync(o, serialized);
}

export async function tiledMapFromFile(filename: string) {
    const file = await readTiledFile(filename);
    return new TiledMap(file);
}

export async function tiledMapLoadTilesetImages(data: TiledRawJSON) {
    return await Promise.all(
        data.tilesets.map(async (tiles) => {
            let file = "";
            if (tiles["source"]) {
                const tilesetFile = await readFile(
                    "assets/tilesets/" + path.basename(tiles.source),
                    "utf-8"
                );
                // HACK hard path
                file =
                    "assets/tilesets/" +
                    path.basename(JSON.parse(tilesetFile).image);
            } else if (tiles["image"]) {
                file = "assets/tilesets/" + path.basename(tiles.image);
            } else {
                return undefined;
            }
            return jimp.read(file);
        })
    );
}
