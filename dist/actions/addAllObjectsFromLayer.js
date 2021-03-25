import 'phaser';
import createObjectsFromLayer from '../factories/createObjectsFromLayer';
import defaults from '../defaults';
export default function (level, layer, xOffset, yOffset) {
    let objdepth = level.scene.game.registry.get('depthmap').get(layer);
    objdepth = objdepth === undefined ? defaults.charDepth : objdepth;
    let mos = createObjectsFromLayer(level.map, layer, objdepth, xOffset || 0, yOffset || 0);
    let arr = [];
    for (let obj of mos) {
        level.scene.add.existing(obj);
        arr.push(obj);
    }
    return arr;
}
//# sourceMappingURL=addAllObjectsFromLayer.js.map