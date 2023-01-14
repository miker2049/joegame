import {applyPixelWangs} from './png2tilemap'

import { createEmptyTiledMap } from './utils'
import { readTiledFile} from './utils-node'
import fs from 'fs/promises'
import jimp from 'jimp'

const WANGSIZE = 4

;(async function() {
    let img = await jimp.read("assets/maps/desert/meta-map-sm.png")
    const stamps = await readTiledFile("assets/maps/desert/desert-stamps.json")
    const worldWidth = WANGSIZE * img.bitmap.width
    const worldHeight = WANGSIZE * img.bitmap.height
    let map = createEmptyTiledMap(stamps, worldWidth, worldHeight)
    for(let i = 0; i < stamps.layers.length; i++){
        let maybemap = applyPixelWangs(stamps, WANGSIZE, map, i,img)
        if(maybemap) map = maybemap
        else console.log(stamps.layers[i].name)
    }
    await fs.writeFile('assets/maps/desert/ttmap.json', JSON.stringify(map))
})()
