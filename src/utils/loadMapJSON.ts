import 'phaser'
import { getMapKeyNameRaw } from './getKeyNames'
import loadAfterLoad from './loadAfterLoad'

export default function(game: Phaser.Game, path: string) {
    return new Promise<Phaser.Game>((res, reject) => {
        let scene = game.scene.getScenes(true, false)[0]
        loadAfterLoad(scene, getMapKeyNameRaw(path), path).then(key => res(game)).catch(err => console.log(err))
    })
}
