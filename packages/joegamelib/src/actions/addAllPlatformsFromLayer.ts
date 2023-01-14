import defaults from '../defaults'
import 'phaser'
import createPlatformsFromLayer from '../factories/createPlatformsFromLayer'
import { ILevelComponents } from '../ILevel'

export default function (level: ILevelComponents, layer: string): void {
  const depth =
    level.scene.game.registry.get('depthmap').get(layer) ??
    defaults.platformDepth
  const plats = createPlatformsFromLayer(level, layer, depth)

  for (const plat of plats) {
    level.scene.add.existing(plat)
    level.platforms.add(plat)
  }
}
