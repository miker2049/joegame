import 'phaser'
import createAnimsFromSheet from './createAnimsFromSheet'
import wikiData from './wikiData'
import defaults from '../defaults'
/*
 * generate all animations from whatever textures happened to be loaded up
 * ASSUMES wikidata is loaded
 */
export default function createAnims(game: Phaser.Game) {
    for (let t in game.textures.list) {
        const sp = wikiData(game).spritesheet.get(t)
        if (sp) {
            createAnimsFromSheet(sp.key, sp.animLength || defaults.animLength, game)
        }
    }
}
