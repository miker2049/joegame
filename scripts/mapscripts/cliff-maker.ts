import { match } from "assert";
import { basename } from "path";
import { ILayer } from "../../src/types/TiledRawJson";
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

        iterateGrid(t.clone(), (x, y, v) => {
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


        const rowIdMap = {}
        baseMap.getLayers().forEach(l => {
            const match = l.name.match(new RegExp(`${mainLayer}_(\\d)`))
            if (match) {
                const row = parseInt(match[1])
                return rowIdMap[row] = l.id
            }
        })

        const replacers = getReplacementSet(templateMap, mainLayer)
        baseMap.getLayers().forEach(l => {
            let bname = l.name.split('_')[0]
            if (bname == mainLayer) {
                applyTiledReplacements(baseMap, l.id, replacers)
            }
        })


        console.log(rowIdMap)

        const fmain = baseMap.addEmptyLayer('fmain')
        const fbehind = baseMap.addEmptyLayer('fbehind')
        iterateGrid(t, (x, y, val) => {
            const res = evaluateCliffLayerTiles(t, x, y, 3)
            const _lgi = rowIdMap[`${y}`]
            console.log(_lgi)
            for (let layer in baseMap.getLayers().filter(l =>
                l.name.match(new RegExp(`${mainLayer}_\\d`)))) {
                layer = baseMap.getLayers()[layer].id
                switch (res) {
                    case CliffReduceOptions.behind: {
                        const sect = getSubArr(x * 4, y * 4, 4, 4, baseMap.lg[layer])
                        addChunk(baseMap.lg[fbehind],
                            sect, x * 4, y * 4, 0)
                        break
                    }
                    case CliffReduceOptions.main: {
                        const sect = getSubArr(x * 4, y * 4, 4, 4, baseMap.lg[layer])
                        addChunk(baseMap.lg[fmain],
                            sect, x * 4, y * 4, 0)
                        break
                    }
                    case CliffReduceOptions.drop: {
                        const sect = new DataGrid(Array(4 * 4).fill(0), 4)
                        // addChunk(baseMap.lg[fmain],
                        //     sect, x * 4, y * 4, 0)

                    }

                }
            }
        })
        baseMap.updateDimensionsFromLayer(mainIndex)
        baseMap.write('assets/maps/desert/testcliffs2.json')
    })()

export enum CliffReduceOptions { drop, main, behind }
export function evaluateCliffLayerTiles(cg: Grid<number>, x: number, y: number, max: number): CliffReduceOptions {
    if (isCliffOccluded(cg, x, y, max)) {
        if (cg.at(x, y) > 1) return CliffReduceOptions.drop
        else return CliffReduceOptions.behind
    } else {
        return CliffReduceOptions.main
    }
}

function isCliffOccluded(cg: Grid<number>, x: number, y: number, max: number, n: number = 0) {
    if (n > max) return false
    const below = cg.at(x, y - 1)
    if (!below) {
        return isCliffOccluded(cg, x, y - 1, max, n + 1)
    } else {
        if (below > 1) {
            return true
        } else {
            return false
        }
    }
}
