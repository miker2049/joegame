
import { ILevelComponents } from '../ILevel'

/*
 * This is a function to add npcs to a map from a layer
 */
export default function(level: ILevelComponents, layer: string): void {

    if (!level.map.getObjectLayer(layer)) { return }
    // first, create an object with keys that correspond to the npc names
    // that have values which are sets of locations in absolute pixels
    for (let obj_ of level.map.getObjectLayer(layer).objects) {
        level.scene.lights.addLight(obj_.x,obj_.y, 300)
    }
}
