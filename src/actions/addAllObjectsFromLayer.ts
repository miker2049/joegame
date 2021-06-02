import 'phaser'
import createObjectsFromLayer from '../factories/createObjectsFromLayer'
import defaults from '../defaults'
import { ILevelComponents } from '../ILevel'
import { IMapObject } from 'components/MapObject'

export default function(
    level: ILevelComponents,
    layer: string,
    xOffset?: number,
    yOffset?: number): IMapObject[] {
    // let objdepth=level.scene.game.registry.get('depthmap').get(layer)
    // objdepth = objdepth === undefined ? defaults.charDepth : objdepth
    // TODO fix the depth thing
    const objdepth = defaults.charDepth
    let mos = createObjectsFromLayer(level.map, layer, objdepth, xOffset || 0, yOffset || 0)
    let arr: IMapObject[] = []
    for (let obj of mos) {
        level.scene.add.existing(obj)
        arr.push(obj)
    }
    return arr
}
