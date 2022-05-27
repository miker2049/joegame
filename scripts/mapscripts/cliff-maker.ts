import { addChunk, applyTiledReplacements, DataGrid, getReplacementSet, getSubArr, Grid, growGridVertical, iterateGrid } from "./mapscript-utils";
import { TiledMap } from "./TiledMap";

/*
 * # input file is just a tiled json
 * If a layer has a
 */

const mainLayer = 'main'
const fileIn = '/home/mik/projects/joegame/assets/maps/desert/desert-cliff-stamps_bc.json'

const cliffMax = 3

    ; (async () => {
        const n = 8
        const t_data = Array(n ** 2).fill(0).map(_ => Math.floor(Math.random() * cliffMax))
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

        iterateGrid(t.clone(), (x, y, v) => {
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


        let newIds = []
        for (let i = 0; i < cliffMax+1; i++) {
            const id = baseMap.addEmptyLayer(`final_${mainLayer}_${i}`)
            newIds.push(id)
        }
        iterateGrid(t, (x, y, val) => {
            let n = 0
            baseMap.getLayers().forEach(l => {
                let bname = l.name.split('_')[0]
                if (bname == mainLayer) {
                    const sect = getSubArr(x * 4, y * 4, 4, 4, baseMap.lg[l.id])
                    if (sect.isEmpty(0) === false) {
                        console.log(newIds[n])
                        addChunk(baseMap.lg[newIds[n]],sect,x*4,y*4,0)
                        n+=1
                    }
                }
            })
        })

        baseMap.updateDimensionsFromLayer(mainIndex)
        baseMap.write('assets/maps/desert/testcliffs2.json')
    })()

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
