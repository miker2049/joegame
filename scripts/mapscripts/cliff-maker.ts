import { addChunk, DataGrid, findAndReplaceAllGrid, getGrowComponents, getMinMaxGrid, getSubArr, Grid, growGridVertical, iterateGrid, ReplacementSet } from "./mapscript-utils";

/*
 * # input file is just a tiled json
 * If a layer has a
 */

export function getCurrentHeight(grid: Grid, x: number, y: number){
    let out = grid.at(x,y)
    if(out===0) return out

    while(y<grid.height()){
        y +=1
        const below = grid.at(x,y)
        if(below<out){
            out = below
            break
        }
        else if(below>out) continue
        else if(below === out) continue
    }
    return out
}

export function applyCliffs(templateGrid: Grid, name: string,
    altitudeMap: Grid, replacementSets: ReplacementSet[]) {

    let finalGrids = []
    const [_, altMax] = getMinMaxGrid(altitudeMap)
    const top = getSubArr(0,0,templateGrid.width,4,growGridVertical(1, 3, templateGrid, 0))
    const bottom = getSubArr(0,3,templateGrid.width,4,growGridVertical(4,3,templateGrid,0))
    for (let i = 0; i <= altMax; i++) {
        finalGrids[i-1]=DataGrid.createEmpty(altitudeMap.width * 4, altitudeMap.height() * 4, 0)
        iterateGrid(altitudeMap,(x,y,v)=>{
            if(v === i)
               addChunk(finalGrids[i-1],top,x*4,(y-i)*4,0)
            else if(v > i)
               addChunk(finalGrids[i-1],bottom,x*4,(y-i)*4,0)
        })
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
