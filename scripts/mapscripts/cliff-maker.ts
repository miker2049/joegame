import { addChunk, DataGrid, findAndReplaceAllGrid, getMinMaxGrid, getSubArr, Grid, growGridVertical, iterateGrid, ReplacementSet } from "./mapscript-utils";

/*
 * # input file is just a tiled json
 * If a layer has a
 */
export function applyCliffs(templateGrid: Grid, name: string,
    altitudeMap: Grid, replacementSets: ReplacementSet[]) {
    const rowLayers: Grid<number>[] = []
    // console.log(altitudeMap.print())
    // console.log(altitudeMap.width,altitudeMap.height())
    for (let i = 0; i < altitudeMap.height(); i++) {
        // let idx = baseMap.addEmptyLayer(mainLayer + "_" + i)
        rowLayers[i] = DataGrid.createEmpty(altitudeMap.width * 4, altitudeMap.height() * 4, 0)
    }

    iterateGrid(altitudeMap.clone(), (x, y, v) => {
        if (v===undefined) return
        // v -= 1
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
        // console.log(rowLayers[y] ? rowLayers[y].width : "nope" , y)
        addChunk(rowLayers[y],
            thisChunk,
            thisX , thisY - (chunkHeight - 4), 0)
    })

    let finalGrids = []
    const [_, altMax] = getMinMaxGrid(altitudeMap)
    for (let i = 0; i < altMax + 1; i++) {
        finalGrids.push(DataGrid.createEmpty(altitudeMap.width * 4, altitudeMap.height() * 4, 0))
    }

    iterateGrid(altitudeMap, (x, y, val) => {
        for (let pos = 0; pos <= val; pos++) {
            if (y - pos < 0) return
            const sect = getSubArr(x * 4, (y - pos) * 4, 4, 4, rowLayers[y])
                 //TODO move the x offset to a param
            addChunk(finalGrids[pos], sect, (x * 4)-0, (y - pos) * 4, 0)
        }
    })
    finalGrids.forEach(grd => {
        replacementSets.forEach(sset => findAndReplaceAllGrid(sset[0], sset[1], grd))
    })
    // finalGrids.pop() //FIXME: do we need this really?
    return finalGrids
}
