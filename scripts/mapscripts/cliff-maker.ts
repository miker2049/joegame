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
        if (v === undefined || v <= 0) return // 0 is ground level, sea floor, so no altitude
        // if v is 1, it is a zero cliff, which takes up a quad and doesn't
        // displace a color quad. Both a zero and a 1 then, should not displace a tile

        const ogVHeight = v
        const belowVal = altitudeMap.at(x,y+1)

        // if(belowVal) v = Math.max(0,v - belowVal)

        let thisChunk = growGridVertical((v - 1) * 4, 3, templateGrid, 0)
        const wouldBeChunk = growGridVertical((ogVHeight - 1) * 4, 3, templateGrid, 0)
        let chunkHeight = thisChunk.height()
        const thisX = x * 4
        const thisY = y * 4
        if (chunkHeight > (thisY + 4)) {
            let diff = chunkHeight - (thisY + 4)
            let clipHeight = chunkHeight - diff
            thisChunk = getSubArr(0, diff, thisChunk.width, clipHeight, thisChunk)
            chunkHeight = thisChunk.height()
        }
        // console.log(rowLayers[y] ? rowLayers[y].width : "nope" , y)
        addChunk(rowLayers[y],
            thisChunk,
            thisX , (thisY - chunkHeight) + 4, 0)
    })

    let finalGrids = []
    const [_, altMax] = getMinMaxGrid(altitudeMap)
    for (let i = 0; i < altMax + 1; i++) {
        finalGrids.push(DataGrid.createEmpty(altitudeMap.width * 4, altitudeMap.height() * 4, 0))
    }

    /*
     * At this point, the tile columns are separated across rows where they are based.
     * Going through the altMap, the job here is to add cliff quads to the altitude
     * they are actually on. It reads a val from x: 3, y:4 with the value 3, so it knows that three quads
     * from y rowLayer should go on y - the height n/offset, so put the base xy quad on grid[0], second on grid[1]
     * and so on.
     * grid[0] = 3,4
     *
     */

    iterateGrid(altitudeMap, (x, y, val) => {
        for (let pos = 0; pos <= val; pos++) {
            if (y - pos < 0) return
            const sect = getSubArr(x * 4, (y - pos) * 4, 4, 4, rowLayers[y])
            addChunk(finalGrids[pos], sect, (x * 4), ((y - pos) * 4), 0)
        }
    })
    finalGrids.forEach(grd => {
        replacementSets.forEach(sset => findAndReplaceAllGrid(sset[0], sset[1], grd))
    })
    return finalGrids
}
