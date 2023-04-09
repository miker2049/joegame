// -*- lsp-enabled-clients: (deno-ls); -*-
import { readFile, writeFile, rm } from "node:fs/promises";
import TiledRawJSON from "../../joegamelib/src/types/TiledRawJson.d.ts";
import { TiledMap } from "./TiledMap.ts";
import * as path from "node:path";
import jimp from "npm:jimp";
import { exec } from "node:child_process";
import { tmpdir } from "node:os";
import { jprng } from "./hasher.ts";

export async function readTiledFile(p: string): Promise<TiledRawJSON> {
    const fi = await readFile(p, "utf-8");
    return JSON.parse(fi);
}

export async function tiledMapFromFile(filename: string) {
    const file = await readTiledFile(filename);
    return new TiledMap(file);
}

export async function tiledMapLoadTilesetImages(
    data: TiledRawJSON
): Promise<jimp[]> {
    return (
        await Promise.all(
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
                const out = await jimp.read(file);
                if (!out) throw new Error("Could not load tileset image");
                return out as jimp;
            })
        )
    ).filter((x) => !!x) as jimp[];
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
