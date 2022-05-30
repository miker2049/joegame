import { expect } from "chai"
import { reduceAltitudeMapCol  } from  "../../scripts/mapscripts/cliff-maker"
import fs from 'fs'
import { pixelsToWang2Corners } from "../../scripts/mapscripts/png2tilemap"
import { TiledMap } from "../../scripts/mapscripts/TiledMap"
import { attachTileChunks, DataGrid, getSubArr, addChunk, injectChunk, gridCopy, checkForRectangle, collectSubArr, encodeGrid, getMaxXofGrid, makeEmptyGrid, printGrid, addLayer, gridFromRegionCode, growGridVertical, findInGrid, findAndReplaceAllGrid, snapNormalGrid} from "../../scripts/mapscripts/mapscript-utils"

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
        // const collected = pixelsToWang2Corners(grid, 1)
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

    it('will grow according to negative offsets',()=>{
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
            [1,1,1,0,0],
            [1,1,1,0,0],
            [1,1,1,0,0],
            [1,1,1,0,0],
            [1,1,1,0,0],
            [0,0,0,0,0],
            [0,0,0,0,0],
            [0,0,0,0,0],
            [0,0,0,0,0],
            [0,0,0,0,0],
        ] )
        const res2 = DataGrid.fromGrid( [
            [1,1,1,0,0,0,0,0],
            [1,1,1,0,0,0,0,0],
            [1,1,1,0,0,0,0,0],
            [1,1,1,0,0,0,0,0],
            [1,1,1,0,0,0,0,0],
        ] )
        const res3 = DataGrid.fromGrid( [
            [1,1,1,0,0,0,0,0],
            [1,1,1,0,0,0,0,0],
            [1,1,1,0,0,0,0,0],
            [1,1,1,0,0,0,0,0],
            [1,1,1,0,0,0,0,0],
            [0,0,0,0,0,0,0,0],
            [0,0,0,0,0,0,0,0],
            [0,0,0,0,0,0,0,0],
            [0,0,0,0,0,0,0,0],
            [0,0,0,0,0,0,0,0],
        ] )
        const res4 = DataGrid.fromGrid( [
            [1,1,1,0,0,0,0],
            [1,1,1,0,0,0,0],
            [1,1,1,0,0,0,0],
            [1,1,1,0,0,0,0],
            [1,1,1,0,0,0,0],
        ])
        const grid3 = DataGrid.fromGrid( [
            [1,1,1],
            [1,1,1],
        ])
        const grid4 = DataGrid.fromGrid( [
            [2,2],
            [2,2],
        ])
        const res5 = DataGrid.fromGrid([
            [0,2,2],
            [0,2,2],
            [1,1,1],
            [1,1,1],
        ])
        const out = addChunk(grid1.clone(),grid2.clone(),0,-5,0)
        const out2 = addChunk(grid1.clone(),grid2.clone(),-3,0,0)
        const out3 = addChunk(grid1.clone(),grid2.clone(),-3,-5,0)
        const out4 = addChunk(grid1.clone(),grid2.clone(),-2,0,0)
        const out5 = addChunk(grid3.clone(), grid4.clone(),1,-2,0)
        expect(out.getData()).to.eql(res.getData())
        expect(out2.getData()).to.eql(res2.getData())
        expect(out3.getData()).to.eql(res3.getData())
        expect(out4.getData()).to.eql(res4.getData())
        expect(out5.getData()).to.eql(res5.getData())
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
        expect(addChunk(g,r,0,0,undefined)
            .getData()).to.eql(r.getData())
        expect(addChunk(r1.clone(),r2,1,0,undefined)
            .getData()).to.eql(rr.getData())
        const grid1 = DataGrid.fromGrid([
            [0,0,0,0,0],
            [0,0,0,0,0],
            [0,0,0,0,0],
            [0,0,0,0,0],
            [0,0,0,0,0],
        ])
        const grid2 = DataGrid.fromGrid([
            [1,1,1],
            [1,1,1],
            [1,1,1],
            [1,1,1],
            [1,1,1],
        ])

        const res = DataGrid.fromGrid([
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
        ])

        const res2 = DataGrid.fromGrid([
            [1,1,1,0,0,0,0,0,0,1,1,1],
            [1,1,1,0,0,0,0,0,0,1,1,1],
            [1,1,1,0,0,0,0,0,0,1,1,1],
            [1,1,1,0,0,0,0,0,0,1,1,1],
            [1,1,1,0,0,0,0,0,0,1,1,1],
        ])
        const res3 = DataGrid.fromGrid([
            [1,1,1,0,0,0,0,0,0,0,0,0],
            [1,1,1,0,0,0,0,0,0,1,1,1],
            [1,1,1,0,0,0,0,0,0,1,1,1],
            [1,1,1,0,0,0,0,0,0,1,1,1],
            [1,1,1,0,0,0,0,0,0,1,1,1],
            [0,0,0,0,0,0,0,0,0,1,1,1]
        ])

        const out = addChunk(grid1,grid2,6,6,0)
        expect(out.getData()).to.eql(res.getData())
        expect(addChunk(grid2.clone(),grid2,9,0,0).getData()).to.eql(res2.getData())
        expect(addChunk(grid2.clone(),grid2,9,1,0).getData()).to.eql(res3.getData())
    })
})



describe("TiledMap", ()=>{
    const template = JSON.parse(fs.readFileSync("assets/maps/empty.json", 'utf8'))
    it("shares the same data between config and grid 'view'", ()=>{
        let tm = TiledMap.createEmpty(3,3, template )
        tm.addEmptyLayer('test')
        tm.lg[0].setVal(2,2,420)
        const conf = tm.getConf()
        expect(conf.layers[0].data[8]).to.eq(420)
        tm.addEmptyLayer('test2')
        tm.lg[1].setVal(0,0,69)
        expect(conf.layers[0].data[8]).to.eq(420)
        expect(conf.layers[1].data[0]).to.eq(69)
        expect(tm.lg[1].at(0,0)).to.eq(69)
        expect(tm.lg[0].at(2,2)).to.eq(420)
    })
})

describe('getSubArr',()=>{
    it('returns the correct sub array',()=>{
        const base = DataGrid.fromGrid([
            [1,2,0,0],
            [3,4,0,0],
        ])
        const res = DataGrid.fromGrid([
            [1,2],
            [3,4]
        ])
        const res2 = DataGrid.fromGrid([
            [0,0],
            [0,0]
        ])
        expect(getSubArr(0,0,2,2,base).getData()).to.eql(res.getData())
        expect(getSubArr(2,0,2,2,base).getData()).to.eql(res2.getData())
        // expect(gridFromRegionCode(0b0011, base).getData()).to.eql(row.getData())

    })
})
describe('gridFromRegionCode', () => {
    const base = DataGrid.fromGrid([
        [1, 2, 0, 0],
        [3, 4, 0, 0],
    ])
    const rowt = DataGrid.fromGrid([
        [1, 2, 0, 0]
    ])
    const row = DataGrid.fromGrid([
        [3, 4, 0, 0]
    ])
    const col = DataGrid.fromGrid([
        [1],
        [3],
    ])
    it('returns the correct grid for a valid code', () => {
        expect(gridFromRegionCode(0b00001111, base).getData()).to.eql(row.getData())
        expect(gridFromRegionCode(0b1111, base).getData()).to.eql(row.getData())
        expect(gridFromRegionCode(0b11110000, base).getData()).to.eql(rowt.getData())
        expect(gridFromRegionCode(0b10001000, base).getData()).to.eql(col.getData())
    })
    it('clips the code to the region so it wont ever error', () => {
        expect(_=>gridFromRegionCode(0b11111100000000001111, base)).to.not.throw
    })
})

describe('growGridVertical', () => {
    const base = DataGrid.fromGrid([
        [1, 1, 1, 1],
        [2, 2, 2, 2],
        [3, 3, 3, 3],
    ])
    const res = DataGrid.fromGrid([
        [1, 1, 1, 1],
        [2, 2, 2, 2],
        [2, 2, 2, 2],
        [2, 2, 2, 2],
        [3, 3, 3, 3],
    ])
    const res2 = DataGrid.fromGrid([
        [1, 1, 1, 1],
        [3, 3, 3, 3],
    ])
    it('correctly grows our grid', () => {
        expect(growGridVertical(3,1,base,0).getData()).to.eql(res.getData())
        expect(growGridVertical(1,1,base,0).getData()).to.eql(base.getData())
    })
    it('removes filler altogether if n is 0', () => {
        expect(growGridVertical(0,1,base,0).getData()).to.eql(res2.getData())
    })
})

describe('findInGrid',()=>{

    const base = DataGrid.fromGrid([
        [1, 1, 1, 1],
        [2, 9, 2, 2],
        [3, 10, 3, 72],
    ])
    const match = DataGrid.fromGrid([
        [9],
        [10],
    ])
    const match2 = DataGrid.fromGrid([
        [2],
        [72],
    ])

    it('correctly finds matches in a grid', ()=>{
        const result = findInGrid(match, base)
        expect(result[0][0]).to.eql({x: 1, y: 1})
    })
    it('correctly finds multiple matches in a grid, and are aligned to input matches', ()=>{
        const result = findInGrid([match, match2], base)
        expect(result[0][0]).to.eql({x: 1, y: 1})
        expect(result[1][0]).to.eql({x: 3, y: 1})
    })
})

describe('findAndReplaceAllGrid',()=>{

    const base = DataGrid.fromGrid([
        [1, 1, 1, 3],
        [2, 9, 2, 2],
        [3, 10, 3, 72],
    ])
    const res = DataGrid.fromGrid([
        [1, 1, 1, 3],
        [2, 69, 2, 2],
        [3, 69, 3, 72],
    ])
    const res2 = DataGrid.fromGrid([
        [1, 1, 45, 45],
        [2, 69, 2, 68],
        [3, 69, 3, 68],
    ])
    const match = DataGrid.fromGrid([
        [9],
        [10],
    ])
    const match2 = DataGrid.fromGrid([
        [2],
        [72],
    ])
    const match3 = DataGrid.fromGrid([
        [1,3],
    ])
    const replacement = DataGrid.fromGrid([
        [69],
        [69],
    ])
    const replacement2 = DataGrid.fromGrid([
        [68],
        [68],
    ])
    const replacement3 = DataGrid.fromGrid([
        [45,45],
    ])

    it('correctly finds matches in a grid and replaces them', ()=>{
        findAndReplaceAllGrid(match, replacement, base)
        expect(base.getData()).to.eql(res.getData())
    })
    it('multiple matchers and replacers', ()=>{
        findAndReplaceAllGrid([match,match2,match3], [replacement, replacement2, replacement3], base)

        expect(base.getData()).to.eql(res2.getData())
    })
})

describe('reduce altitude map',()=>{
    it('returns the correct array filter map thing',()=>{
        const base = DataGrid.fromGrid([
            [2,2],
            [1,1],
            [1,2],
            [3,3],
        ])
        const res = [
            [0],
            [3,1],
            [3,2],
            [3]
        ]
        const res2 = [
            [0],
            [3,2,1],
            [3,2],
            [3]
        ]
        const t = reduceAltitudeMapCol(base, 0)
        const r = reduceAltitudeMapCol(base, 1)
        expect(t).to.eql(res)
        expect(r).to.eql(res2)
        // expect(reduceAltitudeMapCol(base, 0)).to.eql(res.getData())
        // expect(gridFromRegionCode(0b0011, base).getData()).to.eql(row.getData())

    })
})

describe("snapNormalGrid", ()=>{
    it("produces expected results", ()=>{
        const g: DataGrid<number> = DataGrid.fromGrid([
            [0.01,0.9],
            [0.4,0.01]
        ])
        const r = DataGrid.fromGrid([
            [0,2],
            [1,0]
        ])
        expect(snapNormalGrid(g, 3).getData()).to.be.eql(r.getData())
    })
})
