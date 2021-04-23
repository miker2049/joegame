import 'phaser';
import { getMapKeyNameRaw } from './getKeyNames';
import loadAfterLoad from './loadAfterLoad';
export default function (game, path) {
    return new Promise((res, reject) => {
        let scene = game.scene.getScenes(true, false)[0];
        loadAfterLoad(scene, getMapKeyNameRaw(path), path).then(key => res(game)).catch(err => { reject(`Cant find specified map!`); });
    });
}
//# sourceMappingURL=loadMapJSON.js.map