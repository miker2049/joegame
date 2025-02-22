import 'phaser'
import loadAfterLoad from './loadAfterLoad'

export default function(game: Phaser.Game) {
    return new Promise<Phaser.Game>((res, reject) => {
        if (!(game.cache.json.exists('gdata'))) reject("No global data loaded")
        let scene = game.scene.getScenes(true, false)[0]
        loadAfterLoad(scene, 'convo-manifest', scene.cache.json.get('gdata').convoManifest, 'json')
            .then(key => {
                res(game)
            })
            .catch(err => { reject(`Something wrong in retrieving convo manifest`) })
    })
}
