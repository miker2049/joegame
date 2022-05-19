import { expect } from "chai"
import {pixelsToWang2Corners} from "../../scripts/mapscripts/png2tilemap"
import {attachTileChunks, addChunk, injectChunk, gridCopy, checkForRectangle, collectSubArr, encodeGrid, getMaxXofGrid, makeEmptyGrid, printGrid} from "../../scripts/mapscripts/mapscript-utils"

describe("encodeArray and getMask", ()=>{
    it("encoding gives expected results from input", ()=>{
        expect(encodeGrid([
            [0,0,0],
            [0,0,0],
            [0,0,0],
        ], 1)).to.eq(0)
        expect(encodeGrid([
            [1,0,0],
            [0,1,0],
            [0,0,1],
        ], 1)).to.eq(273)
        expect(encodeGrid([
            [0,0,0],
            [0,1,0],
            [0,1,1],
        ], 1)).to.eq(19)
        expect(encodeGrid([
            [1,0,0],
            [0,0,0],
            [0,0,0],
        ], 1)).to.eq(256)
        expect(encodeGrid([
            [1,0,0],
            [0,0,1],
            [0,0,0],
        ], 1)).to.eq(264)
    })
    it("is such that there is commutativeness of encodeArray and getMask ", ()=>{

    })
})


describe('collectSubArr', ()=>{
    it('predicatably collects subarrays',()=>{
        const grid = [
            [1,2,3,4,5,6,7,8],
            [1,2,3,4,5,6,7,8],
            [1,2,3,4,5,6,7,8],
            [1,2,3,4,5,6,7,8],
            [1,2,3,4,5,6,7,8],
            [1,2,3,4,5,6,7,8],
            [1,2,3,4,5,6,7,8],
            [1,2,3,4,5,6,7,8],
        ]
        const collected = collectSubArr(4,4,grid)
        expect(collected[0][0][1]).to.eq(2)
        expect(collected[1][0][1]).to.eq(6)
        expect(collected[2][0][1]).to.eq(2)
        expect(collected[3][0][1]).to.eq(6)
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
    it('can get max x of a grid',()=>{
        const arr = [
            [1,2],
            [1,2],
            [1,2],
            [1,2,3],
            [1,2,3],
            [1,2,3,4],
            [1,2],
        ]
        expect(getMaxXofGrid(arr)).to.eq(4)
    })

    it('can gen a suitable grid', ()=>{
        expect(makeEmptyGrid(3,3,0)).to.deep.eq([
            [0,0,0],
            [0,0,0],
            [0,0,0]
        ])
    })
    it('ovelap simply',()=>{
        const g = [
            [0,0],
            [0,0],
        ]
        const r = [
            [1,2],
            [3,4],
        ]
        const r1 = [
            [0,1,2],
            [0,3,4],
        ]
        const r2 = [
            [0,0,0],
            [0,2,0],
        ]
        expect(attachTileChunks(g,r,0,0,0)).to.eql(r)
        expect(attachTileChunks(g,r,1,0,0)).to.eql(r1)
        expect(attachTileChunks(g,r2,0,0,0)).to.eql(r2)
    })

    it('fast injects',()=>{
        const g = [
            [0,0],
            [0,0],
        ]
        const r = [
            [1,2],
            [3,4],
        ]
        const r1 = [
            [0,1,2],
            [0,3,4],
        ]
        const r2 = [
            [0,6],
            [2,9],
        ]
        const rr = [
            [0,0,6],
            [0,2,9],
        ]
        const rr2 = [
            [0,1,0],
            [0,3,2],
        ]
        expect(injectChunk(g,r,0,0)).to.eql(r)
        expect(injectChunk(gridCopy(r1),r2,1,0)).to.eql(rr)
        expect(injectChunk(gridCopy(r1),r2,2,0)).to.eql(rr2)
    })

    it('will grow according to offsets',()=>{
        const grid1 = [
            [0,0,0,0,0],
            [0,0,0,0,0],
            [0,0,0,0,0],
            [0,0,0,0,0],
            [0,0,0,0,0],
        ]
        const grid2 = [
            [1,1,1],
            [1,1,1],
            [1,1,1],
            [1,1,1],
            [1,1,1],
        ]
        // const res = [
        //     [0,0,0,0,0,undefined,undefined,undefined,undefined],
        //     [0,0,0,0,0,undefined,undefined,undefined,undefined],
        //     [0,0,0,0,0,undefined,undefined,undefined,undefined],
        //     [0,0,0,0,0,undefined,undefined,undefined,undefined],
        //     [0,0,0,0,0,undefined,undefined,undefined,undefined],
        //     [undefined,undefined,undefined,undefined,undefined,undefined,undefined,undefined,undefined],
        //     [undefined,undefined,undefined,undefined,undefined,undefined,1,1,1],
        //     [undefined,undefined,undefined,undefined,undefined,undefined,1,1,1],
        //     [undefined,undefined,undefined,undefined,undefined,undefined,1,1,1],
        //     [undefined,undefined,undefined,undefined,undefined,undefined,1,1,1],
        //     [undefined,undefined,undefined,undefined,undefined,undefined,1,1,1],
        // ]
        const res = [
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
        ]

        const res2 = [
            [1,1,1,0,0,0,0,0,0,1,1,1],
            [1,1,1,0,0,0,0,0,0,1,1,1],
            [1,1,1,0,0,0,0,0,0,1,1,1],
            [1,1,1,0,0,0,0,0,0,1,1,1],
            [1,1,1,0,0,0,0,0,0,1,1,1],
        ]
        const res3 = [
            [1,1,1,0,0,0,0,0,0,0,0,0],
            [1,1,1,0,0,0,0,0,0,1,1,1],
            [1,1,1,0,0,0,0,0,0,1,1,1],
            [1,1,1,0,0,0,0,0,0,1,1,1],
            [1,1,1,0,0,0,0,0,0,1,1,1],
            [0,0,0,0,0,0,0,0,0,1,1,1]
        ]

        const out = attachTileChunks(grid1,grid2,6,6,0)
        expect(out).to.eql(res)
        expect(attachTileChunks(grid2,grid2,9,0,0)).to.eql(res2)
        expect(attachTileChunks(grid2,grid2,9,1,0)).to.eql(res3)
    })

    it('dynamic add chunks',()=>{
        const g = [
            [0,0],
            [0,0],
        ]
        const r = [
            [1,2],
            [3,4],
        ]
        const r1 = [
            [0,1,2],
            [0,3,4],
        ]
        const r2 = [
            [0,6],
            [2,9],
        ]
        const rr = [
            [0,0,6],
            [0,2,9],
        ]
        const rr2 = [
            [0,1,0],
            [0,3,2],
        ]
        expect(addChunk(g,r,0,0)).to.eql(r)
        expect(addChunk(gridCopy(r1),r2,1,0)).to.eql(rr)

        const grid1 = [
            [0,0,0,0,0],
            [0,0,0,0,0],
            [0,0,0,0,0],
            [0,0,0,0,0],
            [0,0,0,0,0],
        ]
        const grid2 = [
            [1,1,1],
            [1,1,1],
            [1,1,1],
            [1,1,1],
            [1,1,1],
        ]

        const res = [
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
        ]

        const res2 = [
            [1,1,1,0,0,0,0,0,0,1,1,1],
            [1,1,1,0,0,0,0,0,0,1,1,1],
            [1,1,1,0,0,0,0,0,0,1,1,1],
            [1,1,1,0,0,0,0,0,0,1,1,1],
            [1,1,1,0,0,0,0,0,0,1,1,1],
        ]
        const res3 = [
            [1,1,1,0,0,0,0,0,0,0,0,0],
            [1,1,1,0,0,0,0,0,0,1,1,1],
            [1,1,1,0,0,0,0,0,0,1,1,1],
            [1,1,1,0,0,0,0,0,0,1,1,1],
            [1,1,1,0,0,0,0,0,0,1,1,1],
            [0,0,0,0,0,0,0,0,0,1,1,1]
        ]

        const out = addChunk(grid1,grid2,6,6,0)
        expect(out).to.eql(res)
        expect(addChunk(gridCopy(grid2),grid2,9,0,0)).to.eql(res2)
        expect(addChunk(gridCopy(grid2),grid2,9,1,0)).to.eql(res3)
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
