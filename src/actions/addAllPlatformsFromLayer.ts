import 'phaser'
import createPlatformsFromLayer from '../factories/createPlatformsFromLayer'
import { ILevelComponents } from '../ILevel'

export default function (level: ILevelComponents, layer: string): void{
    const plats = createPlatformsFromLayer(level, layer)
    for(const plat of plats){
        level.scene.add.existing(plat)
        level.platforms.add(plat)
    }
}
