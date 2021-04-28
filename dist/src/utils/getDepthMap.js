import 'phaser';
import { getMapKeyNameRaw } from './getKeyNames';
export default function (game, mapjsonpath) {
    const mapjson = game.cache.json.get(getMapKeyNameRaw(mapjsonpath));
    const layers = mapjson.layers.map((l, i) => [l.name, i]);
    const map = new Map(layers);
    game.registry.remove('depthmap');
    game.registry.set('depthmap', map);
    return map;
}
//# sourceMappingURL=getDepthMap.js.map