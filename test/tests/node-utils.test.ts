import { expect } from "chai"
import {collectSubArr, encodeGrid, pixelsToWang2Corners} from "../../scripts/mapscripts/png2tilemap"

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
