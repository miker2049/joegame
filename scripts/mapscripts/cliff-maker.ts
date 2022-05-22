import { DataGrid, iterateGrid, getTiledLayerIndex, addChunk, growGridVertical } from "./mapscript-utils";
import { TiledMap } from "./TiledMap";

/*
 * # input file is just a tiled json
 * If a layer has a
 */

const n = 64
const layer_name = 'main'
const fileIn = '/home/mik/projects/joegame/assets/maps/desert/desert-cliff-stamps.json'

const fileOut = 'tt420.json'


;(async ()=>{
    const t_data = Array(n**2).map( _=>Math.floor(Math.random()*n));
    const t = new DataGrid(t_data, n)
    const templateMap = await TiledMap.fromFile(fileIn)
    const index = getTiledLayerIndex(templateMap.getConf(),layer_name) ?? 0
    const templateGrid = templateMap.lg[index]
    const baseMap = TiledMap.createEmpty(64*4,64*4,templateMap.getConf())

    iterateGrid(t,(x,y,v)=>{
        console.log(v)
         baseMap.lg[index]=addChunk(baseMap.lg[index],
                 growGridVertical(v,3,templateGrid,0),
                 x*4,y*4,0 )
    })
    baseMap.updateDimensionsFromLayer(index)
    baseMap.write('testcliffs.json')

})()
