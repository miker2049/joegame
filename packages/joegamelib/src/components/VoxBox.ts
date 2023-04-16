import { GameObjectInWorld } from '../joegameTypes'
import 'phaser'
import TextBox from './TextBox'

const TILEWIDTH = 16

export default class VoxBox extends TextBox {
  constructor(owner: GameObjectInWorld) {
    super({
      fontSize: 16,
      width: TILEWIDTH * 7,
      height: TILEWIDTH * 4,
      alpha: 0.7,
      color: 'black',
      fontColor: 'white',
      x: 0,
      y: 4,
      text: '',
      originX: 0.5,
      originY: 1,
      paddingX: 2,
      paddingY: 2,
      lineN: 3,
      scale: 1 / (owner.scene.cameras.default.zoom * 4),
      level: owner.scene,
      owner
    })

    this.setOrigin(0.5, 0.5)
  }

  preUpdate(_time: number, _delta: number) {
    // super.preUpdate(time, delta)
    if (this.active && this.owner) {
      this.setPosition(this.owner.x + 8, this.owner.y - this.owner.body.height)
    }
  }
}
