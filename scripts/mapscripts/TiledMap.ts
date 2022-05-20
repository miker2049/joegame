import TiledRawJSON from "../../src/types/TiledRawJson";
import { DataGrid, Grid, createEmptyTiledMap, createLayer } from "./mapscript-utils";

export class TiledMap {
    lg: Grid<number>[] //layer grids
    constructor(private config: TiledRawJSON) {
        this.lg = []
        this.initLgs()
    }

    initLgs() {
        this.lg = this.config.layers.map(layer => new DataGrid(layer.data, layer.width))
    }

    getConf(): TiledRawJSON {
        return this.config
    }

    updateConf(input: Partial<TiledRawJSON>) {
        this.config = Object.assign(this.config, input)
        this.initLgs()
    }

    getLayers() {
        return this.config.layers
    }

    addEmptyLayer(name: string) {
        const layer = createLayer(this.config.width,
            this.config.height,
            name,
            this.config.layers.length)
        this.config.layers.push(layer)
        this.initLgs()
    }

    static createEmpty(height: number, width: number, template: TiledRawJSON) {
        return new TiledMap(createEmptyTiledMap(template, width, height))
    }
}
