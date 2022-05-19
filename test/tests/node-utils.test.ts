import { expect } from "chai"
import {pixelsToWang2Corners} from "../../scripts/mapscripts/png2tilemap"
import {attachTileChunks, DataGrid, addChunk, injectChunk, gridCopy, checkForRectangle, collectSubArr, encodeGrid, getMaxXofGrid, makeEmptyGrid, printGrid} from "../../scripts/mapscripts/mapscript-utils"

describe("encodeArray and getMask", ()=>{
    it("encoding gives expected results from input", ()=>{
        expect(encodeGrid(DataGrid.fromGrid( [
            [0,0,0],
            [0,0,0],
            [0,0,0],
        ] ), 1)).to.eq(0)
        expect(encodeGrid(DataGrid.fromGrid( [
            [1,0,0],
            [0,1,0],
            [0,0,1],
        ] ), 1)).to.eq(273)
        expect(encodeGrid(DataGrid.fromGrid( [
            [0,0,0],
            [0,1,0],
            [0,1,1],
        ] ), 1)).to.eq(19)
        expect(encodeGrid(DataGrid.fromGrid( [
            [1,0,0],
            [0,0,0],
            [0,0,0],
        ] ), 1)).to.eq(256)
        expect(encodeGrid(DataGrid.fromGrid( [
            [1,0,0],
            [0,0,1],
            [0,0,0],
        ] ), 1)).to.eq(264)
    })
    it("is such that there is commutativeness of encodeArray and getMask ", ()=>{

    })
})


describe('collectSubArr', ()=>{
    it('predicatably collects subarrays',()=>{
        const grid = DataGrid.fromGrid( [
            [1,2,3,4,5,6,7,8],
            [1,2,3,4,5,6,7,8],
            [1,2,3,4,5,6,7,8],
            [1,2,3,4,5,6,7,8],
            [1,2,3,4,5,6,7,8],
            [1,2,3,4,5,6,7,8],
            [1,2,3,4,5,6,7,8],
            [1,2,3,4,5,6,7,8],
        ] )
        const collected = collectSubArr(4,4,grid)
        expect(collected[0].at(1, 0)).to.eq(2)
        expect(collected[1].at(1, 0)).to.eq(6)
        expect(collected[2].at(1, 0)).to.eq(2)
        expect(collected[3].at(1, 0)).to.eq(6)
        expect(collected.length).to.eq(4)
    })
})

describe('pixelsToWang2', ()=>{
    it('return array of proper tile indexes',()=>{
        const grid = [
            [0,1,3,4,5,6,7,8],
            [1,2,3,4,5,6,7,8],
            [1,2,3,4,5,6,7,8],
            [1,2,3,4,5,6,7,8],
            [1,2,3,4,5,6,7,8],
            [1,2,3,4,5,6,7,8],
            [1,2,3,4,5,6,7,8],
            [1,2,3,4,5,6,7,8],
        ]
        const collected = pixelsToWang2Corners(grid, 1)
    })
})

describe('attachTileChunks and helpers', ()=>{
    it.skip('can get max x of a grid',()=>{
        const arr = DataGrid.fromGrid([
            [1,2],
            [1,2],
            [1,2],
            [1,2,3],
            [1,2,3],
            [1,2,3,4],
            [1,2],
        ])
        expect(getMaxXofGrid(arr)).to.eq(4)
    })

    it('can gen a suitable grid', ()=>{
        expect(makeEmptyGrid(3,3,0).getData()).to.deep.eq([
            [0,0,0],
            [0,0,0],
            [0,0,0]
        ].flat())
    })
    it('ovelap simply',()=>{
        const g = DataGrid.fromGrid( [
            [0,0],
            [0,0],
        ] )
        const r = DataGrid.fromGrid( [
            [1,2],
            [3,4],
        ] )
        const r1 = DataGrid.fromGrid( [
            [0,1,2],
            [0,3,4],
        ] )
        const r2 = DataGrid.fromGrid( [
            [0,0,0],
            [0,2,0],
        ] )
        expect(attachTileChunks(g,r,0,0,0).getData()).to.eql(r.getData())
        expect(attachTileChunks(g,r,1,0,0).getData()).to.eql(r1.getData())
        expect(attachTileChunks(g,r2,0,0,0).getData()).to.eql(r2.getData())
    })

    it('fast injects',()=>{
        const g = DataGrid.fromGrid([
            [0,0],
            [0,0],
        ])
        const r = DataGrid.fromGrid([
            [1,2],
            [3,4],
        ])
        const r1 =DataGrid.fromGrid( [
            [0,1,2],
            [0,3,4],
        ])
        const r2 = DataGrid.fromGrid([
            [0,6],
            [2,9],
        ])
        const rr = DataGrid.fromGrid([
            [0,0,6],
            [0,2,9],
        ])
        const rr2 = DataGrid.fromGrid([
            [0,1,0],
            [0,3,2],
        ])
        expect(injectChunk(g,r,0,0).getData()).to.eql(r.getData())
        expect(injectChunk(r1.clone(),r2,1,0).getData()).to.eql(rr.getData())
        expect(injectChunk(r1.clone(),r2,2,0).getData()).to.eql(rr2.getData())
    })

    it('will grow according to offsets',()=>{
        const grid1 = DataGrid.fromGrid( [
            [0,0,0,0,0],
            [0,0,0,0,0],
            [0,0,0,0,0],
            [0,0,0,0,0],
            [0,0,0,0,0],
        ] )
        const grid2 = DataGrid.fromGrid( [
            [1,1,1],
            [1,1,1],
            [1,1,1],
            [1,1,1],
            [1,1,1],
        ] )
        const res = DataGrid.fromGrid( [
            [0,0,0,0,0,0,0,0,0],
            [0,0,0,0,0,0,0,0,0],
            [0,0,0,0,0,0,0,0,0],
            [0,0,0,0,0,0,0,0,0],
            [0,0,0,0,0,0,0,0,0],
            [0,0,0,0,0,0,0,0,0],
            [0,0,0,0,0,0,1,1,1],
            [0,0,0,0,0,0,1,1,1],
            [0,0,0,0,0,0,1,1,1],
            [0,0,0,0,0,0,1,1,1],
            [0,0,0,0,0,0,1,1,1],
        ] )

        const res2 = DataGrid.fromGrid( [
            [1,1,1,0,0,0,0,0,0,1,1,1],
            [1,1,1,0,0,0,0,0,0,1,1,1],
            [1,1,1,0,0,0,0,0,0,1,1,1],
            [1,1,1,0,0,0,0,0,0,1,1,1],
            [1,1,1,0,0,0,0,0,0,1,1,1],
        ] )
        const res3 = DataGrid.fromGrid( [
            [1,1,1,0,0,0,0,0,0,0,0,0],
            [1,1,1,0,0,0,0,0,0,1,1,1],
            [1,1,1,0,0,0,0,0,0,1,1,1],
            [1,1,1,0,0,0,0,0,0,1,1,1],
            [1,1,1,0,0,0,0,0,0,1,1,1],
            [0,0,0,0,0,0,0,0,0,1,1,1]
        ] )

        const out = attachTileChunks(grid1.clone(),grid2.clone(),6,6,0)
        expect(out.getData()).to.eql(res.getData())
        const out2 = attachTileChunks(grid2.clone(),grid2.clone(),9,0,0)
        expect(out2.getData()).to.eql(res2.getData())
        expect(attachTileChunks(grid2.clone(),grid2.clone(),9,1,0).getData()).to.eql(res3.getData())
    })

    it('dynamic add chunks',()=>{
        const g = DataGrid.fromGrid( [
            [0,0],
            [0,0],
        ] )
        const r = DataGrid.fromGrid( [
            [1,2],
            [3,4],
        ] )
        const r1 = DataGrid.fromGrid( [
            [0,1,2],
            [0,3,4],
        ] )
        const r2 = DataGrid.fromGrid( [
            [0,6],
            [2,9],
        ] )
        const rr = DataGrid.fromGrid( [
            [0,0,6],
            [0,2,9],
        ] )
        const rr2 = DataGrid.fromGrid( [
            [0,1,0],
            [0,3,2],
        ] )
        expect(addChunk(g,r,0,0).getData()).to.eql(r.getData())
        expect(addChunk(r1.clone(),r2,1,0).getData()).to.eql(rr.getData())

        const grid1 = DataGrid.fromGrid( [
            [0,0,0,0,0],
            [0,0,0,0,0],
            [0,0,0,0,0],
            [0,0,0,0,0],
            [0,0,0,0,0],
        ] )
        const grid2 = DataGrid.fromGrid( [
            [1,1,1],
            [1,1,1],
            [1,1,1],
            [1,1,1],
            [1,1,1],
        ] )

        const res = DataGrid.fromGrid( [
            [0,0,0,0,0,0,0,0,0],
            [0,0,0,0,0,0,0,0,0],
            [0,0,0,0,0,0,0,0,0],
            [0,0,0,0,0,0,0,0,0],
            [0,0,0,0,0,0,0,0,0],
            [0,0,0,0,0,0,0,0,0],
            [0,0,0,0,0,0,1,1,1],
            [0,0,0,0,0,0,1,1,1],
            [0,0,0,0,0,0,1,1,1],
            [0,0,0,0,0,0,1,1,1],
            [0,0,0,0,0,0,1,1,1],
        ] )

        const res2 = DataGrid.fromGrid( [
            [1,1,1,0,0,0,0,0,0,1,1,1],
            [1,1,1,0,0,0,0,0,0,1,1,1],
            [1,1,1,0,0,0,0,0,0,1,1,1],
            [1,1,1,0,0,0,0,0,0,1,1,1],
            [1,1,1,0,0,0,0,0,0,1,1,1],
        ] )
        const res3 = DataGrid.fromGrid( [
            [1,1,1,0,0,0,0,0,0,0,0,0],
            [1,1,1,0,0,0,0,0,0,1,1,1],
            [1,1,1,0,0,0,0,0,0,1,1,1],
            [1,1,1,0,0,0,0,0,0,1,1,1],
            [1,1,1,0,0,0,0,0,0,1,1,1],
            [0,0,0,0,0,0,0,0,0,1,1,1]
        ] )

        const out = addChunk(grid1,grid2,6,6,0)
        expect(out.getData()).to.eql(res.getData())
        expect(addChunk(grid2.clone(),grid2,9,0,0).getData()).to.eql(res2.getData())
        expect(addChunk(grid2.clone(),grid2,9,1,0).getData()).to.eql(res3.getData())
    })
})

describe("checkForRectangle",()=>{
   const g = [
       [0,0,0,0,0,0,0,0,0,0],
       [0,0,0,0,0,0,1,0,0,0],
       [0,0,0,0,0,1,1,1,1,0],
       [0,0,0,0,1,1,1,1,1,0],
       [0,0,0,0,1,1,1,1,1,0],
       [0,0,0,0,0,0,0,0,0,0],
   ]
    it.skip("Gives predictable results",()=>{
    expect(checkForRectangle(g,1)).to.eql([[6,1,6,1],[5,2,8,4],[4,3,4,4]])
    })

})
