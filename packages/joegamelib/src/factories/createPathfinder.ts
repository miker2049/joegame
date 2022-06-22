import * as easystar from 'easystarjs';

export default function(map: Phaser.Tilemaps.Tilemap): easystar.js {
    let finder = new easystar.js()
    let mapgrid: number[][] = [];
    let acceptableTiles = new Set<number>();
    for(let y = 0; y < map.height; y++){
        let col: number[] = [];
        for(let x = 0; x < map.width; x++){
            //NOTE hardcoded layer to get collision info
            const tile_ = map.getTileAt(x,y, false, "Main");
            //check if collide
            // if there is tile (that is not empty and not collides) and additionally none of the other tiles have collision,
            if (tile_) {
                const startIndex = map.layers.findIndex((l)=>l.name === 'Main')
                const layers = map.layers.slice(startIndex) //ignore lower layers
                const abovetiles: number[] = layers.map((l)=>{
                    const ltile = map.getTileAt(x,y,true,l.name).properties?.collides
                    return ltile === true ? 1 : 0
                })
                if(abovetiles.reduce((p,v)=>{return p+v})===0){
                    col.push(1)
                } else {
                    col.push(0)
                }
            } else {
                col.push(0)
            }

        }
        mapgrid.push(col);
    }
    finder.setGrid(mapgrid)
    finder.setAcceptableTiles([1])
    return finder
}
