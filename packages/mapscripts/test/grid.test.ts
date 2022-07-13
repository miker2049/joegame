import test from 'tape'
import {
    addChunk,
    attachTileChunks,
    collectSubArr,
    DataGrid,
    encodeGrid,
    injectChunk,
    makeEmptyGrid,
    unflat
} from '../src/mapscript-utils'

import { altMapToCliffLayers } from '../src/cliff-maker'


test("encoding grid", (t) => {
    t.equal(encodeGrid(DataGrid.fromGrid([
        [0, 0, 0],
        [0, 0, 0],
        [0, 0, 0],
    ]), 1), 0)
    t.equal(encodeGrid(DataGrid.fromGrid([
        [1, 0, 0],
        [0, 1, 0],
        [0, 0, 1],
    ]), 1), 273)
    t.equal(encodeGrid(DataGrid.fromGrid([
        [0, 0, 0],
        [0, 1, 0],
        [0, 1, 1],
    ]), 1), 19)
    t.equal(encodeGrid(DataGrid.fromGrid([
        [1, 0, 0],
        [0, 0, 0],
        [0, 0, 0],
    ]), 1), 256)
    t.equal(encodeGrid(DataGrid.fromGrid([
        [1, 0, 0],
        [0, 0, 1],
        [0, 0, 0],
    ]), 1), 264, "two apart")
    t.end()
});

test('collectSubArr predicatably collects subarrays', (t) => {
    const grid = DataGrid.fromGrid([
        [1, 2, 3, 4, 5, 6, 7, 8],
        [1, 2, 3, 4, 5, 6, 7, 8],
        [1, 2, 3, 4, 5, 6, 7, 8],
        [1, 2, 3, 4, 5, 6, 7, 8],
        [1, 2, 3, 4, 5, 6, 7, 8],
        [1, 2, 3, 4, 5, 6, 7, 8],
        [1, 2, 3, 4, 5, 6, 7, 8],
        [1, 2, 3, 4, 5, 6, 7, 8],
    ])
    const collected = collectSubArr(4, 4, grid)
    t.equal(collected[0].at(1, 0), 2)
    t.equal(collected[1].at(1, 0), 6)
    t.equal(collected[2].at(1, 0), 2)
    t.equal(collected[3].at(1, 0), 6)
    t.equal(collected.length, 4)
    t.end()
})


test('makeEmptyGrid function', (t) => {
    t.deepEqual(makeEmptyGrid(3, 3, 0).getData(), [
        [0, 0, 0],
        [0, 0, 0],
        [0, 0, 0]
    ].flat())
    t.end()
})

test('attachTileChunks will overlap grids', (t) => {
    const g = DataGrid.fromGrid([
        [0, 0],
        [0, 0],
    ])
    const r = DataGrid.fromGrid([
        [1, 2],
        [3, 4],
    ])
    const r1 = DataGrid.fromGrid([
        [0, 1, 2],
        [0, 3, 4],
    ])
    const r2 = DataGrid.fromGrid([
        [0, 0, 0],
        [0, 2, 0],
    ])
    t.deepEqual(attachTileChunks(g, r, 0, 0, 0).getData(), r.getData())
    t.deepEqual(attachTileChunks(g, r, 1, 0, 0).getData(), r1.getData())
    t.deepEqual(attachTileChunks(g, r2, 0, 0, 0).getData(), r2.getData())
    t.end()
})

test('fast injects', (t) => {
    const g = DataGrid.fromGrid([
        [0, 0],
        [0, 0],
    ])
    const r = DataGrid.fromGrid([
        [1, 2],
        [3, 4],
    ])
    const r1 = DataGrid.fromGrid([
        [0, 1, 2],
        [0, 3, 4],
    ])
    const r2 = DataGrid.fromGrid([
        [0, 6],
        [2, 9],
    ])
    const rr = DataGrid.fromGrid([
        [0, 0, 6],
        [0, 2, 9],
    ])
    const rr2 = DataGrid.fromGrid([
        [0, 1, 0],
        [0, 3, 2],
    ])
    t.deepEqual(injectChunk(g, r, 0, 0).getData(), r.getData())
    t.deepEqual(injectChunk(r1.clone(), r2, 1, 0).getData(), rr.getData())
    t.deepEqual(injectChunk(r1.clone(), r2, 2, 0).getData(), rr2.getData())
    t.end()
})


test('attachTileChunks will grow according to offsets', (t) => {
    const grid1 = DataGrid.fromGrid([
        [0, 0, 0, 0, 0],
        [0, 0, 0, 0, 0],
        [0, 0, 0, 0, 0],
        [0, 0, 0, 0, 0],
        [0, 0, 0, 0, 0],
    ])
    const grid2 = DataGrid.fromGrid([
        [1, 1, 1],
        [1, 1, 1],
        [1, 1, 1],
        [1, 1, 1],
        [1, 1, 1],
    ])
    const res = DataGrid.fromGrid([
        [0, 0, 0, 0, 0, 0, 0, 0, 0],
        [0, 0, 0, 0, 0, 0, 0, 0, 0],
        [0, 0, 0, 0, 0, 0, 0, 0, 0],
        [0, 0, 0, 0, 0, 0, 0, 0, 0],
        [0, 0, 0, 0, 0, 0, 0, 0, 0],
        [0, 0, 0, 0, 0, 0, 0, 0, 0],
        [0, 0, 0, 0, 0, 0, 1, 1, 1],
        [0, 0, 0, 0, 0, 0, 1, 1, 1],
        [0, 0, 0, 0, 0, 0, 1, 1, 1],
        [0, 0, 0, 0, 0, 0, 1, 1, 1],
        [0, 0, 0, 0, 0, 0, 1, 1, 1],
    ])

    const res2 = DataGrid.fromGrid([
        [1, 1, 1, 0, 0, 0, 0, 0, 0, 1, 1, 1],
        [1, 1, 1, 0, 0, 0, 0, 0, 0, 1, 1, 1],
        [1, 1, 1, 0, 0, 0, 0, 0, 0, 1, 1, 1],
        [1, 1, 1, 0, 0, 0, 0, 0, 0, 1, 1, 1],
        [1, 1, 1, 0, 0, 0, 0, 0, 0, 1, 1, 1],
    ])
    const res3 = DataGrid.fromGrid([
        [1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0],
        [1, 1, 1, 0, 0, 0, 0, 0, 0, 1, 1, 1],
        [1, 1, 1, 0, 0, 0, 0, 0, 0, 1, 1, 1],
        [1, 1, 1, 0, 0, 0, 0, 0, 0, 1, 1, 1],
        [1, 1, 1, 0, 0, 0, 0, 0, 0, 1, 1, 1],
        [0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1]
    ])

    const out = attachTileChunks(grid1.clone(), grid2.clone(), 6, 6, 0)
    t.deepEqual(out.getData(), res.getData())
    const out2 = attachTileChunks(grid2.clone(), grid2.clone(), 9, 0, 0)
    t.deepEqual(out2.getData(), res2.getData())
    t.deepEqual(attachTileChunks(grid2.clone(), grid2.clone(), 9, 1, 0).getData(), res3.getData())
    t.end()
})

test('attachTileChunks will grow according to negative offsets', (t) => {
    const grid1 = DataGrid.fromGrid([
        [0, 0, 0, 0, 0],
        [0, 0, 0, 0, 0],
        [0, 0, 0, 0, 0],
        [0, 0, 0, 0, 0],
        [0, 0, 0, 0, 0],
    ])
    const grid2 = DataGrid.fromGrid([
        [1, 1, 1],
        [1, 1, 1],
        [1, 1, 1],
        [1, 1, 1],
        [1, 1, 1],
    ])
    const res = DataGrid.fromGrid([
        [1, 1, 1, 0, 0],
        [1, 1, 1, 0, 0],
        [1, 1, 1, 0, 0],
        [1, 1, 1, 0, 0],
        [1, 1, 1, 0, 0],
        [0, 0, 0, 0, 0],
        [0, 0, 0, 0, 0],
        [0, 0, 0, 0, 0],
        [0, 0, 0, 0, 0],
        [0, 0, 0, 0, 0],
    ])
    const res2 = DataGrid.fromGrid([
        [1, 1, 1, 0, 0, 0, 0, 0],
        [1, 1, 1, 0, 0, 0, 0, 0],
        [1, 1, 1, 0, 0, 0, 0, 0],
        [1, 1, 1, 0, 0, 0, 0, 0],
        [1, 1, 1, 0, 0, 0, 0, 0],
    ])
    const res3 = DataGrid.fromGrid([
        [1, 1, 1, 0, 0, 0, 0, 0],
        [1, 1, 1, 0, 0, 0, 0, 0],
        [1, 1, 1, 0, 0, 0, 0, 0],
        [1, 1, 1, 0, 0, 0, 0, 0],
        [1, 1, 1, 0, 0, 0, 0, 0],
        [0, 0, 0, 0, 0, 0, 0, 0],
        [0, 0, 0, 0, 0, 0, 0, 0],
        [0, 0, 0, 0, 0, 0, 0, 0],
        [0, 0, 0, 0, 0, 0, 0, 0],
        [0, 0, 0, 0, 0, 0, 0, 0],
    ])
    const res4 = DataGrid.fromGrid([
        [1, 1, 1, 0, 0, 0, 0],
        [1, 1, 1, 0, 0, 0, 0],
        [1, 1, 1, 0, 0, 0, 0],
        [1, 1, 1, 0, 0, 0, 0],
        [1, 1, 1, 0, 0, 0, 0],
    ])
    const grid3 = DataGrid.fromGrid([
        [1, 1, 1],
        [1, 1, 1],
    ])
    const grid4 = DataGrid.fromGrid([
        [2, 2],
        [2, 2],
    ])
    const res5 = DataGrid.fromGrid([
        [0, 2, 2],
        [0, 2, 2],
        [1, 1, 1],
        [1, 1, 1],
    ])
    const out = addChunk(grid1.clone(), grid2.clone(), 0, -5, 0)
    const out2 = addChunk(grid1.clone(), grid2.clone(), -3, 0, 0)
    const out3 = addChunk(grid1.clone(), grid2.clone(), -3, -5, 0)
    const out4 = addChunk(grid1.clone(), grid2.clone(), -2, 0, 0)
    const out5 = addChunk(grid3.clone(), grid4.clone(), 1, -2, 0)
    t.deepEqual(out.getData(), res.getData())
    t.deepEqual(out2.getData(), res2.getData())
    t.deepEqual(out3.getData(), res3.getData())
    t.deepEqual(out4.getData(), res4.getData())
    t.deepEqual(out5.getData(), res5.getData())
    t.end()
})

test('dynamic add chunks', (t) => {
    const g = DataGrid.fromGrid([
        [0, 0],
        [0, 0],
    ])
    const r = DataGrid.fromGrid([
        [1, 2],
        [3, 4],
    ])
    const r1 = DataGrid.fromGrid([
        [0, 1, 2],
        [0, 3, 4],
    ])
    const r2 = DataGrid.fromGrid([
        [0, 6],
        [2, 9],
    ])
    const rr = DataGrid.fromGrid([
        [0, 0, 6],
        [0, 2, 9],
    ])
    const rr2 = DataGrid.fromGrid([
        [0, 1, 0],
        [0, 3, 2],
    ])
    t.deepEqual(addChunk(g, r, 0, 0, undefined)
        .getData(), r.getData())
    t.deepEqual(addChunk(r1.clone(), r2, 1, 0, undefined)
        .getData(), rr.getData())
    const grid1 = DataGrid.fromGrid([
        [0, 0, 0, 0, 0],
        [0, 0, 0, 0, 0],
        [0, 0, 0, 0, 0],
        [0, 0, 0, 0, 0],
        [0, 0, 0, 0, 0],
    ])
    const grid2 = DataGrid.fromGrid([
        [1, 1, 1],
        [1, 1, 1],
        [1, 1, 1],
        [1, 1, 1],
        [1, 1, 1],
    ])

    const res = DataGrid.fromGrid([
        [0, 0, 0, 0, 0, 0, 0, 0, 0],
        [0, 0, 0, 0, 0, 0, 0, 0, 0],
        [0, 0, 0, 0, 0, 0, 0, 0, 0],
        [0, 0, 0, 0, 0, 0, 0, 0, 0],
        [0, 0, 0, 0, 0, 0, 0, 0, 0],
        [0, 0, 0, 0, 0, 0, 0, 0, 0],
        [0, 0, 0, 0, 0, 0, 1, 1, 1],
        [0, 0, 0, 0, 0, 0, 1, 1, 1],
        [0, 0, 0, 0, 0, 0, 1, 1, 1],
        [0, 0, 0, 0, 0, 0, 1, 1, 1],
        [0, 0, 0, 0, 0, 0, 1, 1, 1],
    ])

    const res2 = DataGrid.fromGrid([
        [1, 1, 1, 0, 0, 0, 0, 0, 0, 1, 1, 1],
        [1, 1, 1, 0, 0, 0, 0, 0, 0, 1, 1, 1],
        [1, 1, 1, 0, 0, 0, 0, 0, 0, 1, 1, 1],
        [1, 1, 1, 0, 0, 0, 0, 0, 0, 1, 1, 1],
        [1, 1, 1, 0, 0, 0, 0, 0, 0, 1, 1, 1],
    ])
    const res3 = DataGrid.fromGrid([
        [1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0],
        [1, 1, 1, 0, 0, 0, 0, 0, 0, 1, 1, 1],
        [1, 1, 1, 0, 0, 0, 0, 0, 0, 1, 1, 1],
        [1, 1, 1, 0, 0, 0, 0, 0, 0, 1, 1, 1],
        [1, 1, 1, 0, 0, 0, 0, 0, 0, 1, 1, 1],
        [0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1]
    ])

    const out = addChunk(grid1, grid2, 6, 6, 0)
    t.deepEqual(out.getData(), res.getData())
    t.deepEqual(addChunk(grid2.clone(), grid2, 9, 0, 0).getData(), res2.getData())
    t.deepEqual(addChunk(grid2.clone(), grid2, 9, 1, 0).getData(), res3.getData())
    t.end()
})


test('alt map to cliff layers', (t) => {
    const a1 = [
        [0, 0, 0, 0, 0, 0, 0, 0, 0],
        [0, 1, 1, 1, 1, 1, 1, 1, 0],
        [0, 1, 2, 2, 2, 2, 2, 1, 0],
        [0, 1, 2, 3, 3, 3, 2, 1, 0],
        [0, 1, 2, 3, 4, 3, 2, 1, 0],
        [0, 1, 2, 3, 3, 3, 2, 1, 0],
        [0, 1, 2, 2, 2, 2, 2, 1, 0],
        [0, 1, 1, 1, 1, 1, 1, 1, 0],
        [0, 0, 0, 0, 0, 0, 0, 0, 0]
    ]
    // top - 0, cliff - 1, empty - 2
    const res1 = [
        [ // 0
            [0, 0, 0, 0, 0, 0, 0, 0, 0],
            [0, 0, 0, 0, 0, 0, 0, 0, 0],
            [0, 0, 0, 0, 0, 0, 0, 0, 0],
            [0, 0, 0, 0, 0, 0, 0, 0, 0],
            [0, 0, 0, 0, 0, 0, 0, 0, 0],
            [0, 0, 0, 0, 0, 0, 0, 0, 0],
            [0, 0, 0, 0, 0, 0, 0, 0, 0],
            [0, 0, 0, 0, 0, 0, 0, 0, 0],
            [0, 0, 0, 0, 0, 0, 0, 0, 0]
        ],
        [ // 1
            [2, 0, 0, 0, 0, 0, 0, 0, 2],
            [2, 0, 0, 0, 0, 0, 0, 0, 2],
            [2, 0, 0, 0, 0, 0, 0, 0, 2],
            [2, 0, 0, 0, 0, 0, 0, 0, 2],
            [2, 0, 0, 0, 0, 0, 0, 0, 2],
            [2, 0, 0, 0, 0, 0, 0, 0, 2],
            [2, 0, 0, 0, 0, 0, 0, 0, 2],
            [2, 2, 2, 2, 2, 2, 2, 2, 2],
            [2, 2, 2, 2, 2, 2, 2, 2, 2]
        ],
        [ // 2
            [2, 2, 0, 0, 0, 0, 0, 2, 2],
            [2, 2, 0, 0, 0, 0, 0, 2, 2],
            [2, 2, 0, 0, 0, 0, 0, 2, 2],
            [2, 2, 0, 0, 0, 0, 0, 2, 2],
            [2, 2, 0, 0, 0, 0, 0, 2, 2],
            [2, 2, 2, 2, 2, 2, 2, 2, 2],
            [2, 2, 2, 2, 2, 2, 2, 2, 2],
            [2, 2, 2, 2, 2, 2, 2, 2, 2],
            [2, 2, 2, 2, 2, 2, 2, 2, 2]
        ],
        [ // 3
            [2, 2, 2, 0, 0, 0, 2, 2, 2],
            [2, 2, 2, 0, 0, 0, 2, 2, 2],
            [2, 2, 2, 0, 0, 0, 2, 2, 2],
            [2, 2, 2, 2, 2, 2, 2, 2, 2],
            [2, 2, 2, 2, 2, 2, 2, 2, 2],
            [2, 2, 2, 2, 2, 2, 2, 2, 2],
            [2, 2, 2, 2, 2, 2, 2, 2, 2],
            [2, 2, 2, 2, 2, 2, 2, 2, 2],
            [2, 2, 2, 2, 2, 2, 2, 2, 2]
        ],
        [ // 4
            [2, 2, 2, 2, 0, 2, 2, 2, 2],
            [2, 2, 2, 2, 2, 2, 2, 2, 2],
            [2, 2, 2, 2, 2, 2, 2, 2, 2],
            [2, 2, 2, 2, 2, 2, 2, 2, 2],
            [2, 2, 2, 2, 2, 2, 2, 2, 2],
            [2, 2, 2, 2, 2, 2, 2, 2, 2],
            [2, 2, 2, 2, 2, 2, 2, 2, 2],
            [2, 2, 2, 2, 2, 2, 2, 2, 2],
            [2, 2, 2, 2, 2, 2, 2, 2, 2]
        ],
    ]
    t.deepEqual(
        altMapToCliffLayers(DataGrid.fromGrid(a1)),
        res1.map(g => DataGrid.fromGrid(g)),
        "Simple test")



    const a2 = [
        [0, 0, 0, 0],
        [0, 0, 0, 0],
        [0, 2, 2, 0],
        [0, 2, 2, 0],
        [0, 0, 0, 0]
    ]

    const res2 = [
        [ // 0
            [0, 0, 0, 0],
            [0, 0, 0, 0],
            [0, 0, 0, 0],
            [0, 0, 0, 0],
            [0, 0, 0, 0]
        ],
        [ // 1
            [2, 2, 2, 2],
            [2, 1, 1, 2],
            [2, 1, 1, 2],
            [2, 2, 2, 2],
            [2, 2, 2, 2]
        ],
        [ // 2
            [2, 0, 0, 2],
            [2, 0, 0, 2],
            [2, 2, 2, 2],
            [2, 2, 2, 2],
            [2, 2, 2, 2]
        ],

    ]

    t.deepEqual(altMapToCliffLayers(DataGrid.fromGrid(a2)), res2.map(g => DataGrid.fromGrid(g)),
        "Simple test")

    const a3 = [
        [0, 0, 0, 0, 0, 0],
        [0, 1, 1, 1, 1, 0],
        [0, 1, 3, 3, 1, 0],
        [0, 1, 3, 3, 1, 0],
        [0, 1, 1, 1, 1, 0],
        [0, 0, 0, 0, 0, 0]
    ]

    const res3 = [
        [ // 0
            [0, 0, 0, 0, 0, 0],
            [0, 0, 0, 0, 0, 0],
            [0, 0, 0, 0, 0, 0],
            [0, 0, 0, 0, 0, 0],
            [0, 0, 0, 0, 0, 0],
            [0, 0, 0, 0, 0, 0]
        ],
        [ // 1
            [2, 0, 0, 0, 0, 2],
            [2, 0, 0, 0, 0, 2],
            [2, 0, 0, 0, 0, 2],
            [2, 0, 0, 0, 0, 2],
            [2, 2, 2, 2, 2, 2],
            [2, 2, 2, 2, 2, 2]
        ],
        [ // 2
            [2, 2, 1, 1, 2, 2],
            [2, 2, 1, 1, 2, 2],
            [2, 2, 2, 2, 2, 2],
            [2, 2, 2, 2, 2, 2],
            [2, 2, 2, 2, 2, 2],
            [2, 2, 2, 2, 2, 2]
        ],
        [ // 2
            [2, 2, 0, 0, 2, 2],
            [2, 2, 2, 2, 2, 2],
            [2, 2, 2, 2, 2, 2],
            [2, 2, 2, 2, 2, 2],
            [2, 2, 2, 2, 2, 2],
            [2, 2, 2, 2, 2, 2]
        ],

    ]

    t.deepEqual(
        altMapToCliffLayers(DataGrid.fromGrid(a3)),
        res3.map(g => DataGrid.fromGrid(g)),
        "Simple test")
    t.end()
})
