import { addChunk, DataGrid, findAndReplaceAllGrid, getMinMaxGrid, getSubArr, Grid, growGridVertical, iterateGrid, ReplacementSet } from "./mapscript-utils";

/*
 * # input file is just a tiled json
 * If a layer has a
 */

export function getCurrentGround(grid: Grid, x: number, y: number) {
    let out = grid.at(x, y)
    if (out === 0) return out

    while (y < grid.height()) {
        y += 1
        const below = grid.at(x, y)
        if (below < out) {
            out = below
            break
        }
        else if (below > out) continue
        else if (below === out) continue
    }
    return out
}

enum CliffLayerUnits {
    top = 0, cliff = 1, empty = 2
}
type CliffLayer = CliffLayerUnits[][]
export function altMapToCliffLayers(altMap: number[][]): CliffLayer[] {
   let out: CliffLayer[] = []
    return out
}

export function applyCliffs(templateGrid: Grid, name: string,
    altitudeMap: Grid, replacementSets: ReplacementSet[]) {

    const [_, altMax] = getMinMaxGrid(altitudeMap)
    const top = getSubArr(0, 0, templateGrid.width, 4, growGridVertical(1, 3, templateGrid, 0))
    const bottom = getSubArr(0, 3, templateGrid.width, 4, growGridVertical(4, 3, templateGrid, 0))
    let finalGrids: Grid<number>[] = []
    for (let layer = 0; layer <= altMax; layer++) {
        finalGrids[layer] = DataGrid.createEmpty(altitudeMap.width * 4, altitudeMap.height() * 4, 0)
    }
    // Start by iterating through the grid of altitude values, derived from the
    // alpha channel of the base img.
    iterateGrid(altitudeMap, (x, y, v) => {

      // The ground is a special value, representing where the cliff starts
      // from.
      // Consider an altMap:
      //
      //   01234
      //
      // 0 00000
      // 1 01110
      // 2 22210
      // 3 01110
      // 4 00000
      //
      // The one on (1,3) takes up one space at itself ((1,3)) with a
      // top piece.
      //
      // The 2 at (0,2) takes up two spaces, a cliff on (0,2) and top at (0,1).
      // But the 2 at (1,2) is
        const ground = getCurrentGround(altitudeMap, x, y)
        // For every value of our altitude grid, iterate through every layer.
        for (let layer = 0; layer <= altMax; layer++) {
            if(v===layer)
                addChunk(finalGrids[layer],top,x*4,(y-layer + 1)*4,0)
            if (layer === ground && v > 0)
                addChunk(finalGrids[layer],top,x*4,y*4,0)
            if (layer === 0)
                addChunk(finalGrids[layer],top,x*4,y*4,0)
            else if(layer > (ground) && layer < v)
                addChunk(finalGrids[layer],bottom,x*4,(y-layer+1)*4,0)
            else if(layer < ground)
                continue
            else if(layer > v)
                break
        }
    })


    /*
     * At this point, the tile columns are separated across rows where they are based.
     * Going through the altMap, the job here is to add cliff quads to the altitude
     * they are actually on. It reads a val from x: 3, y:4 with the value 3, so it knows that three quads
     * from y rowLayer should go on y - the height n/offset, so put the base xy quad on grid[0], second on grid[1]
     * and so on.
     * grid[0] = 3,4
     *
     */

    // iterateGrid(altitudeMap, (x, y, val) => {
    //     const below = getCurrentHeight(altitudeMap,x,y)
    //     let relativeHeight = val - below
    //     for (let pos = below; pos < val; pos++) {
    //         if(x === 10) console.log(`val: ${val} x: ${x} y: ${y} pos: ${pos } below: ${below} relativeHeight ${relativeHeight}`)
    //         if (y - pos < 0) return
    //         const sect = getSubArr(x * 4, (y - (pos-below)) * 4, 4, 4, rowLayers[y])
    //         addChunk(finalGrids[pos], sect, (x * 4), ((y - (pos-below)) * 4), 0)
    //     }
    // })
    finalGrids.forEach(grd => {
        replacementSets.forEach(sset => findAndReplaceAllGrid(sset[0], sset[1], grd))
    })
    // return finalGrids

    return finalGrids
}
