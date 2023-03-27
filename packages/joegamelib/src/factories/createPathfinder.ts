import * as easystar from 'easystarjs'
type Pathfinder = easystar.js
export function createPathfinder(map: Phaser.Tilemaps.Tilemap): Pathfinder {
  let finder = new easystar.js()
  let mapgrid: number[][] = []
  for (let y = 0; y < map.height; y++) {
    let col: number[] = []
    for (let x = 0; x < map.width; x++) {
      const ct = map.getTileAt(x, y, true, 'COLLIDERS')
      if (ct.index - ct.tileset.firstgid === 23) {
        col.push(0)
      } else {
        col.push(1)
      }
    }
    mapgrid.push(col)
  }
  finder.setGrid(mapgrid)
  finder.setAcceptableTiles([1])
  return finder
}
