import { readTiledFile, createEmptyTiledMap, applyPixelWangs} from './png2tilemap'
import fs from 'fs/promises'
import jimp from 'jimp'

const WANGSIZE = 4

    ; (async function() {
        let img = await jimp.read("assets/maps/desert/meta-map-sm.png")
        const stamps = await readTiledFile("assets/maps/desert/desert-stamps.json")
        const worldWidth = WANGSIZE * img.bitmap.width
        const worldHeight = WANGSIZE * img.bitmap.height
        let map = createEmptyTiledMap(stamps, worldWidth, worldHeight)
        for(let i = 0; i < stamps.layers.length; i++){
            let maybemap = applyPixelWangs(stamps, WANGSIZE, map, i,img)
            if(maybemap) map = maybemap
        }
        await fs.writeFile('assets/maps/desert/ttmap.json', JSON.stringify(map))
        // const qs = SpecialSets
        // checkGridForMatches<number>(pixels,)
        // img.write('img_test.png')
    })()
