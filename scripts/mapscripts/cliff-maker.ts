import { DataGrid, iterateGrid, getTiledLayerIndex, addChunk, growGridVertical } from "./mapscript-utils";
import { TiledMap } from "./TiledMap";

/*
 * # input file is just a tiled json
 * If a layer has a
 */

const layer_name = 'main'
const fileIn = '/home/mik/projects/joegame/assets/maps/desert/desert-cliff-stamps.json'

const fileOut = 'tt420.json'


;(async ()=>{
    const n = 12
    const t_data = Array(n**2).fill(0).map( _=>Math.floor(Math.random()*3)+4)
    console.log(t_data)
    const t = new DataGrid(t_data, n)
    const templateMap = await TiledMap.fromFile(fileIn)
    const index = getTiledLayerIndex(templateMap.getConf(),layer_name) ?? 0
    const templateGrid = templateMap.lg[index]
    const baseMap = TiledMap.createEmpty(n*4,n*4,templateMap.getConf())

    let yOffset = 0
    iterateGrid(t,(x,y,v)=>{
        const thisYOffset = v > 4 ? v - 4 : 0
        const thisY = thisYOffset == 0 ? (y*4) + yOffset : (y*4)-thisYOffset
        if((y*4)-thisYOffset < 0) return
         baseMap.lg[index]=addChunk(baseMap.lg[index],
                 growGridVertical(v,3,templateGrid,0),
                                    x*4,thisY,0 )
        yOffset = Math.max(thisYOffset, yOffset)
    })
    baseMap.updateDimensionsFromLayer(index)
    baseMap.write('assets/maps/desert/testcliffs2.json')
})()
