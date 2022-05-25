import { DataGrid, iterateGrid, getTiledLayerId, addChunk, growGridVertical, getSubArr, checkTiledLayerProperty, Grid, gridFromRegionCode, getReplacementSet, applyTiledReplacements } from "./mapscript-utils";
import { TiledMap } from "./TiledMap";

/*
 * # input file is just a tiled json
 * If a layer has a
 */

const mainLayer = 'main'
const fileIn = '/home/mik/projects/joegame/assets/maps/desert/desert-cliff-stamps_bc.json'



    ; (async () => {
        const n = 8
        const t_data = Array(n ** 2).fill(0).map(_ => Math.floor(Math.random() * 3))
        const t = new DataGrid(t_data, n)
        const templateMap = await TiledMap.fromFile(fileIn)
        // const mainIndex = getTiledLayerIndex(templateMap.getConf(), mainLayer) ?? 0
        const mainIndex = templateMap.getConf().layers.find(l => l.name === mainLayer).id
        const templateGrid = templateMap.lg[mainIndex]
        let baseMap = TiledMap.createEmpty(n * 4, n * 4, templateMap.getConf())
        let rowLayers: number[] = []
        for (let i = 0; i < t.height(); i++) {
            let idx = baseMap.addEmptyLayer(mainLayer + "_" + i)
            rowLayers[i] = idx
        }

        iterateGrid(t, (x, y, v) => {
            let thisChunk = growGridVertical(v * 4, 3, templateGrid, 0)
            let chunkHeight = thisChunk.height()
            let thisX = x * 4
            let thisY = y * 4
            if (chunkHeight > (thisY + 4)) {
                let diff = chunkHeight - (thisY + 4)
                let clipHeight = chunkHeight - diff
                thisChunk = getSubArr(0, diff, thisChunk.width, clipHeight, thisChunk)
                chunkHeight = thisChunk.height()
            }
            addChunk(baseMap.lg[rowLayers[y]],
                thisChunk,
                thisX, thisY - (chunkHeight - 4), 0)
        })
        baseMap.initLgs()

        const replacers = getReplacementSet(templateMap, mainLayer)
        baseMap.getLayers().forEach(l => {
            let bname = l.name.split('_')[0]
            if (bname == mainLayer) {
                applyTiledReplacements(baseMap, l.id, replacers)
            }
        })
        // baseMap.updateDimensionsFromLayer(mainIndex)
        baseMap.write('assets/maps/desert/testcliffs2.json')
    })()
