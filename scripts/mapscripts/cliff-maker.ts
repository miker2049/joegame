import { addChunk, applyTiledReplacements, findAndReplaceAllGrid, checkTiledLayerProperty, DataGrid, getReplacementSet, getSubArr, Grid, gridFromRegionCode, growGridVertical, iterateGrid, ReplacementSet } from "./mapscript-utils";
import { TiledMap } from "./TiledMap";

/*
 * # input file is just a tiled json
 * If a layer has a
 */

const mainLayer = 'main'
const fileIn = '/home/mik/projects/joegame/assets/maps/desert/desert-cliff-stamps_bc.json'

const cliffMax = 3
export function applyCliffs(templateGrid: Grid, name: string,
    altitudeMap: Grid, replacementSets: ReplacementSet[]) {
    const rowLayers: Grid<number>[] = []
    for (let i = 0; i < altitudeMap.height(); i++) {
        // let idx = baseMap.addEmptyLayer(mainLayer + "_" + i)
        rowLayers[i] = DataGrid.createEmpty(altitudeMap.width * 4, altitudeMap.height() * 4, 0)
    }

    iterateGrid(altitudeMap.clone(), (x, y, v) => {
        if (!v) return
        v -= 1
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
        addChunk(rowLayers[y],
            thisChunk,
            thisX, thisY - (chunkHeight - 4), 0)
    })
    let finalGrids = []
    for (let i = 0; i < cliffMax + 1; i++) {
        finalGrids.push(DataGrid.createEmpty(altitudeMap.width * 4, altitudeMap.height() * 4, 0))
    }

    iterateGrid(altitudeMap, (x, y, val) => {
        for (let pos = 0; pos <= val; pos++) {
            if (y - pos < 0) return
            const sect = getSubArr(x * 4, (y - pos) * 4, 4, 4, rowLayers[y])
            addChunk(finalGrids[pos + 1], sect, x * 4, (y - pos) * 4, 0)
        }
    })
    finalGrids.forEach(grd => {
        replacementSets.forEach(sset => findAndReplaceAllGrid(sset[0], sset[1], grd))
    })
    finalGrids.pop() //FIXME: do we need this really?
    return finalGrids
}

// ; (async () => {
//     const n = 8
//     const t_data = Array(n ** 2).fill(0).map(_ => Math.floor(Math.random() * cliffMax))
//     const t = new DataGrid(t_data, n)
//     const templateMap = await TiledMap.fromFile(fileIn)
//     // const mainIndex = getTiledLayerIndex(templateMap.getConf(), mainLayer) ?? 0
//     const mainIndex = templateMap.getConf().layers.find(l => l.name === mainLayer).id
//     let templateGrid = templateMap.lg[mainIndex]
//     const mainRegion = checkTiledLayerProperty(templateMap.getConf(), mainIndex, mainLayer + "-region")
//     const pRegion = mainRegion.split('-').map(i=>parseInt(i))
//     if (mainRegion) {
//         templateGrid = getSubArr(pRegion[0],pRegion[1],pRegion[2],pRegion[3],templateGrid)
//     }
//     let baseMap = TiledMap.createEmpty(n * 4, n * 4, templateMap.getConf())
//     const replacers = getReplacementSet(templateMap, mainLayer)
//     const replacers2 = getReplacementSet(templateMap, mainLayer, 2)
//     const lgs = applyCliffs(templateGrid,mainLayer,t,[replacers,replacers2])
//     baseMap.applyLgs(lgs, "cliff")
//     // baseMap.updateDimensionsFromLayer(mainIndex)
//     baseMap.write('assets/maps/desert/testcliffs2.json')
// })()

export function getCliffLayerGrid(tmap: TiledMap, pos: number, basename: string) {
    const fres = tmap.getLayers().find(i => i.name.match(`${basename}_${pos}`))
    if (fres) {
        return fres
    } else {
        return undefined
    }
}


export function reduceAltitudeMapCol(altitudeMap: Grid<number>, col: number): number[][] {
    let out = Array(altitudeMap.height()).fill(0).map(i => new Array())
    for (let y = altitudeMap.height() - 1; y >= 0; y--) {
        const val = altitudeMap.at(col, y)
        for (let i = val - 1; i >= 0; i--) {
            if (y - i > -1) {
                out[y - i].push(y)
            }
        }
    }
    return out
}
