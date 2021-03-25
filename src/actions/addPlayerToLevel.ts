import 'phaser'
import { ILevelComponents } from '../ILevel'
import createPlayer from '../factories/createPlayer';
import { ICharacter } from '../ICharacter';

export default function(level: ILevelComponents, x: number, y: number, char?: string): ICharacter {
    const player = createPlayer(char ? char : "default", x, y, level)
    level.scene.add.existing(player as Phaser.GameObjects.GameObject)
    level.player = player
    return player
}
