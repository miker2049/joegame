import { GameObjectInWorld } from '../joegameTypes'
import { LevelScene } from '../LevelScene'
import TextBox from './TextBox'

export default class NameLabel extends TextBox {
  constructor(level: LevelScene, text: string, owner?: GameObjectInWorld) {
    super({
      fontSize: 16,
      width: 100,
      height: 20,
      text,
      alpha: 1,
      color: 'black',
      fontColor: 'white',
      x: owner ? owner.x : 0,
      y: 18,
      originX: 0.5,
      originY: 1,
      paddingX: 0,
      paddingY: 0,
      lineN: 1,
      scale: 1 / (level.cameras.default.zoom * 4),
      level,
      owner
    })

    this.style.align = 'center'
    this.close()
  }
}
