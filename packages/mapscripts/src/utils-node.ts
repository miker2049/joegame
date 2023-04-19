// -*- lsp-enabled-clients: (deno-ls); -*-
import TiledRawJSON from "../../joegamelib/src/types/TiledRawJson.d.ts";
import { TiledMap } from "./TiledMap.ts";
// import jimp from "https://esm.sh/jimp@0.22.7";

export async function readTiledFile(p: string): Promise<TiledRawJSON> {
    const fi = await Deno.readTextFile(p);
    return JSON.parse(fi);
}

export async function tiledMapFromFile(filename: string) {
    const file = await readTiledFile(filename);
    return new TiledMap(file);
}

// export async function tiledMapLoadTilesetImages(
//     data: TiledRawJSON
// ): Promise<jimp[]> {
//     return (
//         await Promise.all(
//             data.tilesets.map(async (tiles) => {
//                 let file = "";
//                 if (tiles["source"]) {
//                     const tilesetFile = await Deno.readTextFile(
//                         "assets/tilesets/" + pathBasename(tiles.source)
//                     );
//                     // HACK hard path
//                     file =
//                         "assets/images/" +
//                         pathBasename(JSON.parse(tilesetFile).image);
//                 } else if (tiles["image"]) {
//                     file = "assets/images/" + pathBasename(tiles.image);
//                 } else {
//                     return undefined;
//                 }
//                 const out = await jimp.read(file);
//                 if (!out) throw new Error("Could not load tileset image");
//                 return out as jimp;
//             })
//         )
//     ).filter((x) => !!x) as jimp[];
// }

export async function isValidTilemap(data: TiledRawJSON): Promise<boolean> {
    const tmpath = tmpfile() + ".json";
    const tmpath2 = tmpfile() + ".json";
    await Deno.writeTextFile(tmpath, JSON.stringify(data));
    const proc = Deno.run({
        cmd: [
            "tiled",
            "--resolve-types-and-properties",
            "--embed-tilesets",
            "--export-map",
            "json",
            tmpath,
            tmpath2,
        ],
    });
    const result = await proc.status();
    await Promise.all([
        Deno.remove(tmpath),
        result.success ? Deno.remove(tmpath2) : undefined,
    ]);
    proc.close();
    return result.success;
}

export function tmpfile() {
    return Deno.makeTempFileSync();
}
