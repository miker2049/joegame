import fs from 'fs/promises';
import TiledMap from 'joegamelib/src/types/TiledRawJson';

;(async function (){
    const path: string = process.argv[2]
    let map: TiledMap = JSON.parse(await fs.readFile(path, 'utf-8'))
    console.log(map.layers)
})()
