import { readFileSync, writeFileSync, existsSync } from "fs";
import TiledRawJSON from "joegamelib/src/types/TiledRawJson";
import { embedTilesetsOffline } from "../src/embedTilesetOffline";
import { createPackSection, saturateObjects } from "../src/saturator";

function saturateMap(m: TiledRawJSON) {
    embedTilesetsOffline(m);
    saturateObjects(m);
    return { pack: createPackSection(m), ...m };
}

if (!existsSync(process.argv[2])) {
    console.log("saturate-map.ts <in> <out>");
} else {
    let tm: TiledRawJSON = JSON.parse(readFileSync(process.argv[2], "utf-8"));
    const t = saturateMap(tm);
    writeFileSync(process.argv[3], JSON.stringify(t));
}
