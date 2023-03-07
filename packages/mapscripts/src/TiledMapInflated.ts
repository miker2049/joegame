import TiledRawJSON from "joegamelib/src/types/TiledRawJson";
import { TiledMap } from "./TiledMap";
import { unzlibSync } from "fflate";

function parseCompressed(input: string): number[] {
    try {
        const decoded = new Uint8Array(Buffer.from(input, "base64"));
        const result = unzlibSync(decoded);
        const arr = new Int32Array(result.buffer);
        const out = Array.from(arr);
        return out;
    } catch (err) {
        console.log(err);
    }
}
export class TiledMapInflated extends TiledMap {
    constructor(conf: TiledRawJSON) {
        super(conf);
        this.inflateLayers();
    }
    private inflateLayers() {
        const newLayers = this.getConf().layers.map((l) => {
            if (l.type === "tilelayer" && typeof l.data === "string") {
                return {
                    height: l.height,
                    width: l.width,
                    id: l.id,
                    name: l.name,
                    visible: l.visible,
                    opacity: l.opacity,
                    x: l.x,
                    y: l.y,
                    type: "tilelayer",
                    data: parseCompressed(l.data),
                };
            } else return l;
        });
        this.updateConf({ layers: newLayers });
    }
}
