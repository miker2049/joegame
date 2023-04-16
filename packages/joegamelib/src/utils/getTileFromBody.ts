import 'phaser'
import { AssuredVec2 } from '../joegameTypes'

export default function (
  go: { body: Phaser.Physics.Arcade.Body },
  tilesize: number
): AssuredVec2 {
  if (go.body) {
    const x_ = Math.floor(go.body.center.x / tilesize) * tilesize
    const y_ = Math.floor(go.body.center.y / tilesize) * tilesize
    return { x: x_ / tilesize, y: y_ / tilesize + 1 }
  } else {
    return { x: 0, y: 0 }
  }
}
