import 'phaser'
import ensure from './ensure'

/**
 * Turns a set of points in path into a set of differences between points
 */
export default function (
  path: Phaser.Types.Math.Vector2Like[]
): Phaser.Types.Math.Vector2Like[] {
  let transitions: Phaser.Types.Math.Vector2Like[] = []
  for (let i = 1; i < path.length; i++) {
    const xDiff = ensure(path[i].x) - ensure(path[i - 1].x)
    const yDiff = ensure(path[i].y) - ensure(path[i - 1].y)
    transitions.push({ x: xDiff, y: yDiff })
  }
  return transitions
}
