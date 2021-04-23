import 'phaser';
import shuffle from './shuffleArr';
export default function (map, amount, start, bounds) {
    const boundss = bounds !== null && bounds !== void 0 ? bounds : { x: 0, y: 0, width: map.width, height: map.height };
    const rect = new Phaser.Geom.Rectangle(boundss.x, boundss.y, boundss.width, boundss.height);
    const filtertiles = map.getTilesWithinShape(rect);
    let tiles = [];
    for (let i = 0; i < (amount !== null && amount !== void 0 ? amount : 3) + 1; i++) {
        tiles.push(getRandomValidTile(filtertiles));
    }
    return tiles.map((item, i, arr) => { return { x: item.x, y: item.y }; });
}
function getRandomValidTile(tiles) {
    let tile = shuffle(tiles)[0];
    return tile;
}
//# sourceMappingURL=randomInterestSet.js.map