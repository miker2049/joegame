import {writeFile} from 'fs/promises'
import TiledRawJSON from "../../src/types/TiledRawJson";
import { DataGrid, Grid, createEmptyTiledMap, createLayer, readTiledFile } from "./mapscript-utils";

export class TiledMap {
    lg: Grid<number>[] //layer grids
    constructor(private config: TiledRawJSON) {
        this.lg = []
        this.initLgs()
    }

    applyLgs(lgs: Grid<number>[], basename: string, append: boolean = false) {
        const grids=lgs.map((grd, idx)=>{
            const layer = createLayer(this.config.width,this.config.height,basename+'_'+idx,idx)
            layer.data = grd.getData()
            return layer
        })
        this.config.layers = grids.concat(append ? this.config.layers : [])
        this.config.layers = this.config.layers.map((l, id)=>{
            return {id, ...l}
        })
        this.initLgs()
    }
    initLgs() {
        this.lg = []
        this.config.layers.forEach(layer =>
            this.lg[layer.id] = new DataGrid(layer.data, layer.width))
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
        const id = this.config.layers.find(i=>i.name===name).id
        return id
    }
    updateDimensionsFromLayer(i:number): void {
        this.updateConf({width: this.lg[i].width, height: this.lg[i].height()})
        this.config.layers[i].width = this.lg[i].width
        this.config.layers[i].height = this.lg[i].height()
    }

    async write(path: string): Promise<void> {
        return writeFile(path, JSON.stringify(this.config))
    }

    static createEmpty(height: number, width: number, template: TiledRawJSON) {
        return new TiledMap(createEmptyTiledMap(template, width, height))
    }

    static async fromFile(filename: string) {
        const file = await readTiledFile(filename)
        return new TiledMap(file)
    }


}
