import { expect } from "chai"
import {encodeGrid} from "../../scripts/mapscripts/png2tilemap"

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
