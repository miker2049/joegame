import { readFileSync, writeFileSync } from "fs";
import TiledRawJSON from "joegamelib/src/types/TiledRawJson";
import { saturateMap } from "./saturator";

let tm: TiledRawJSON = JSON.parse(readFileSync(process.argv[2], "utf-8"));
const t = saturateMap(tm);
writeFileSync(process.argv[3], JSON.stringify(t));
