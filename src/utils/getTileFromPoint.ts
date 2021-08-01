import 'phaser'
import {AssuredVec2} from '../joegameTypes'

export default function(go: AssuredVec2, tilesize: number): AssuredVec2 {
    const x_ = (Math.floor((go.x) / tilesize) * tilesize)
    const y_ = (Math.floor((go.y) / tilesize) * tilesize)
    return { x: x_ / tilesize, y: (y_ / tilesize) }
}
