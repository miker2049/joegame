import 'phaser'
import { ILevelComponents } from '../ILevel'
import createPlayer from '../factories/createPlayer';
import { ICharacter } from '../ICharacter';
import defaults from 'defaults';

export default function(level: ILevelComponents, x: number, y: number, char?: string): ICharacter {

    const player = createPlayer(char ? char : "player", x, y, level)
    level.scene.add.existing(player as Phaser.GameObjects.GameObject)
    level.player = player
    // level.player.sprite.on('animationrepeat', () => {
    //     level.toner.play({ inst: 'walk' })

    // })
    return player
}
