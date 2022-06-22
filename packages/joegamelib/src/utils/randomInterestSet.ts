import 'phaser'
import shuffle from './shuffleArr'
export default function(map: Phaser.Tilemaps.Tilemap, amount?: number, start?: { x: number, y: number }, bounds?: { x: number, y: number, width: number, height: number }) {
    const boundss = bounds ?? { x: 0, y: 0, width: map.width, height: map.height }
    const rect = new Phaser.Geom.Rectangle(boundss.x, boundss.y, boundss.width, boundss.height)
    const filtertiles = map.getTilesWithinShape(rect)
    let tiles: any[] = []
    for (let i = 0; i < (amount ?? 3) + 1; i++) {
        tiles.push(getRandomValidTile(filtertiles))
    }
    return tiles.map((item, i, arr) => { return { x: item.x, y: item.y } });
}

function getRandomValidTile(tiles: Phaser.Tilemaps.Tile[]): Phaser.Tilemaps.Tile {
    let tile = shuffle(tiles)[0]
    return tile
}
