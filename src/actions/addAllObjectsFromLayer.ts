import 'phaser'
import createObjectsFromLayer from '../factories/createObjectsFromLayer'
import defaults from '../defaults'
import { ILevelComponents } from '../ILevel'

export default function(level: ILevelComponents, layer: string, xOffset?: number, yOffset?: number): Phaser.GameObjects.Image[]{
    let objdepth=level.scene.game.registry.get('depthmap').get(layer)
    objdepth = objdepth === undefined ? defaults.charDepth : objdepth
    let mos = createObjectsFromLayer(level.map,layer,objdepth, xOffset || 0, yOffset || 0)
    let arr: Phaser.GameObjects.Image[] = []
    for(let obj of mos){
        level.scene.add.existing(obj)
        arr.push(obj)
    }
    return arr
}
