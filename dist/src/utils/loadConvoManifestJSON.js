import 'phaser';
import loadAfterLoad from './loadAfterLoad';
export default function (game) {
    return new Promise((res, reject) => {
        if (!(game.cache.json.exists('gdata')))
            reject("No global data loaded");
        let scene = game.scene.getScenes(true, false)[0];
        loadAfterLoad(scene, 'convo-manifest', scene.cache.json.get('gdata').convoManifest)
            .then(key => {
            res(game);
        })
            .catch(err => { reject(`Something wrong in retrieving convo manifest`); });
    });
}
//# sourceMappingURL=loadConvoManifestJSON.js.map