import { TiledMap } from '../src/TiledMap'
import test from 'tape'
import fs from 'fs'

test("shares the same data between config and grid 'view'", (t) => {
    const template = JSON.parse(fs.readFileSync("./test/empty.json", 'utf8'))
    let tm = TiledMap.createEmpty(3, 3, template)
    tm.addEmptyLayer('test')
    tm.lg[0].setVal(2, 2, 420)
    const conf = tm.getConf()
    t.equal(conf.layers[0].data[8], 420)
    tm.addEmptyLayer('test2')
    tm.lg[1].setVal(0, 0, 69)
    t.equal(conf.layers[0].data[8], 420)
    t.equal(conf.layers[1].data[0], 69)
    t.equal(tm.lg[1].at(0, 0), 69)
    t.equal(tm.lg[0].at(2, 2), 420)
    t.end()
})
