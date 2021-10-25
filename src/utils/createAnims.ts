import defaults from '../defaults'
import createAnimsFromSheet from './createAnimsFromSheet'
import wikiData from './wikiData'
/*
 * generate all animations from whatever textures happened to be loaded up
 * ASSUMES wikidata is loaded
 */
export default function createAnims(game: Phaser.Game, jsonpath: string) {
    for (let t in game.textures.list) {
        const sp = wikiData(game).spritesheet.get(t)
        if (sp) {
            createAnimsFromSheet(sp.key, sp.animLength || defaults.animLength, game)
        }
    }

}
