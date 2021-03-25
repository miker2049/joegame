import 'phaser';
import createPlatformsFromLayer from '../factories/createPlatformsFromLayer';
export default function (level, layer) {
    const plats = createPlatformsFromLayer(level, layer);
    for (const plat of plats) {
        level.scene.add.existing(plat);
        level.platforms.add(plat);
    }
}
//# sourceMappingURL=addAllPlatformsFromLayer.js.map