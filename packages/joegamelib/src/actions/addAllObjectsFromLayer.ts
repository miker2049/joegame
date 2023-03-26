import 'phaser'
import createObjectsFromLayer from '../factories/createObjectsFromLayer'
import { IMapObject } from '../components/MapObject'
import { LevelScene } from '../LevelScene'

export default function (
  scene: LevelScene,
  layer: string,
  xOffset?: number,
  yOffset?: number
): IMapObject[] {
  const objdepth = 10
  // level.scene.game.registry.get('depthmap').get(layer) ?? defaults.charDepth
  // const objdepth = defaults.charDepth
  const mos = createObjectsFromLayer(
    scene,
    layer,
    objdepth,
    xOffset || 0,
    yOffset || 0
  )
  let arr: IMapObject[] = []
  for (let obj of mos) {
    scene.add.existing(obj)
    console.log(obj)
    arr.push(obj)
  }
  return arr
}
