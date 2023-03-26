// -*- lsp-enabled-clients: (deno-ls); -*-
import { finalizeTiledmap } from "./utils.ts";

if (!(await Deno.stat(Deno.args[0]))) {
    console.log("saturate-map.ts <in> <out>");
} else {
    const tm = JSON.parse(await Deno.readTextFile(Deno.args[0]));
    const t = await finalizeTiledmap(tm);
    await Deno.writeTextFile(Deno.args[1], JSON.stringify(t));
}
