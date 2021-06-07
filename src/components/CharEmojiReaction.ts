import defaults from '../defaults'
import { ILevelComponents } from 'ILevel'
import 'phaser'
import emojiMap from '../utils/emojiMap'
import loadAfterLoad from 'utils/loadAfterLoad'

export class CharEmojiReaction extends Phaser.GameObjects.Image {
    constructor(scene: Phaser.Scene,  emoji: string) {
        super(scene, 0, 0, emoji, 0)
    }
}

export async function getEmojiObject(emoji: string, scene: Phaser.Scene){
    let key = await loadAfterLoad(scene,emoji,defaults.emojiPath+(emojiMap.get(emoji)??defaults.emoji),'image')
    return new CharEmojiReaction(scene,key)
}
