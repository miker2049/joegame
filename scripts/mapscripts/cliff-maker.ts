import { DataGrid, iterateGrid, getTiledLayerIndex, addChunk, growGridVertical, getSubArr, checkTiledLayerProperty, Grid, gridFromRegionCode } from "./mapscript-utils";
import { TiledMap } from "./TiledMap";

/*
 * # input file is just a tiled json
 * If a layer has a
 */

const layer_name = 'main'
const fileIn = '/home/mik/projects/joegame/assets/maps/desert/desert-cliff-stamps_bc.json'

const fileOut = 'tt420.json'


;(async ()=>{
    const n = 8
    const t_data = Array(n**2).fill(0).map( _=>Math.floor(Math.random()*3))
    console.log(t_data)
    const t = new DataGrid(t_data, n)
    const templateMap = await TiledMap.fromFile(fileIn)
    const index = getTiledLayerIndex(templateMap.getConf(),layer_name) ?? 0
    const fillerIndex = getTiledLayerIndex(templateMap.getConf(),layer_name + '_filler') ?? 1
    const templateGrid = templateMap.lg[index]
    const baseMap = TiledMap.createEmpty(n*4,n*4,templateMap.getConf())
    let rowLayers: number[] = []
    for(let i =0; i<t.height(); i++){
        let idx = baseMap.addEmptyLayer(layer_name + "_" +i)
        rowLayers[i] = idx
    }

    iterateGrid(t,(x,y,v)=>{
        let thisChunk = growGridVertical(v*4,3,templateGrid,0)
        let chunkHeight = thisChunk.height()
        let thisX = x * 4
        let thisY = y * 4
        if(chunkHeight > (thisY + 4 )){
            let diff = chunkHeight - (thisY + 4 )
            let clipHeight = chunkHeight - diff
            thisChunk = getSubArr(0,diff,thisChunk.width, clipHeight, thisChunk)
            chunkHeight = thisChunk.height()
        }
        baseMap.lg[index]=addChunk(baseMap.lg[rowLayers[y]],
                                    thisChunk,
                                    thisX,thisY-(chunkHeight-4),0 )
    })
    baseMap.updateDimensionsFromLayer(index)
    baseMap.write('assets/maps/desert/testcliffs2.json')
})()
