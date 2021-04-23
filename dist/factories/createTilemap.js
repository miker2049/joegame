import 'phaser';
import { getMapKeyName } from '../utils/getKeyNames';
export default function (scene, mapjsonpath, offsetX, offsetY) {
    const tilemap = scene.make.tilemap({ key: getMapKeyName(mapjsonpath) });
    // const depthmap = scene.game.registry.get('depthmap')
    //initialize tilesets, and also leave a reference to them so they can easily be used in making the layers
    for (let tileset of tilemap.tilesets) {
        // Note that here, to keep things simpler, every tileset is preloaded with the name of the filename itself, so two `tileset.name`s
        if (tileset.total > 1) {
            tilemap.addTilesetImage(tileset.name, tileset.name, tileset.tileWidth, tileset.tileHeight, tileset.tileMargin, tileset.tileSpacing);
        }
    }
    // init all our layers...
    tilemap.layers.forEach((l, i) => {
        tilemap.createLayer(l.name, tilemap.tilesets, offsetX || 0, offsetY || 0);
    });
    tilemap.createBlankLayer('highlight', tilemap.tilesets).setVisible(true);
    // collision for map
    tilemap.layers.forEach((l) => { l.tilemapLayer.setCollisionByProperty({ collides: true }); });
    return tilemap;
}
//# sourceMappingURL=createTilemap.js.map