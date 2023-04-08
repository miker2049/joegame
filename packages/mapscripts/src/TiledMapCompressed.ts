// -*- lsp-enabled-clients: (deno-ls); -*-
import { zlibSync } from "fflate";
import { TiledMapInflated } from "./TiledMapInflated.ts";

export function compressData(d: number[]) {
    const arr = Int32Array.from(d);
    const res = zlibSync(new Uint8Array(arr.buffer), { level: 9 });
    const out = btoa(String.fromCharCode(...Array.from(res)));
    // const out = Buffer.from(res).toString("base64");
    return out;
}
export class TiledMapCompressed extends TiledMapInflated {
    compressLayers() {
        const newLayers = this.getConf().layers.map((l) => {
            if (l.type === "tilelayer" && typeof l.data !== "string") {
                return {
                    ...l,
                    data: compressData(l.data),
                    compression: "zlib",
                    encoding: "base64",
                };
            } else return l;
        });
        this.updateConf({ layers: newLayers });
    }
}
