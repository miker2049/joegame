import { readFileSync, writeFileSync } from "fs";
import { readFile, writeFile, rm } from "fs/promises";
import TiledRawJSON from "joegamelib/src/types/TiledRawJson";
import { parseCSVRowsToGameData } from "joegamelib/src/utils/parseCSVRowsToGameData";
import { TiledMap } from "./TiledMap";
import path from "path";
import jimp from "jimp";
import { exec } from "child_process";
import { tmpdir } from "os";
import { jprng } from "noise/ripemd160";

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

export async function tiledMapLoadTilesetImages(
    data: TiledRawJSON
): Promise<jimp[]> {
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
                    "assets/images/" +
                    path.basename(JSON.parse(tilesetFile).image);
            } else if (tiles["image"]) {
                file = "assets/images/" + path.basename(tiles.image);
            } else {
                return undefined;
            }
            return jimp.read(file);
        })
    );
}

export async function isValidTilemap(data: TiledRawJSON): Promise<boolean> {
    const tmpath = tmpfile() + ".json";
    const tmpath2 = tmpfile() + ".json";
    await writeFile(tmpath, JSON.stringify(data));
    const result = await new Promise<boolean>((res, rej) => {
        exec(
            `tiled --resolve-types-and-properties --embed-tilesets --export-map json ${tmpath} ${tmpath2}`,
            (err, s) => {
                if (err) res(false);
                else res(true);
            }
        );
    });
    await Promise.all([rm(tmpath), result ? rm(tmpath2) : undefined]);
    return result;
}

export function tmpfile() {
    const r = Math.floor(1000000 * jprng(Math.floor(Math.random() * 100), 69));
    return path.join(
        tmpdir(),
        "jgame" + Number(r).toString(16).substring(0, 8)
    );
}
