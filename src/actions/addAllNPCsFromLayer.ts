import { interpret } from 'xstate'
import createNPCsFromLayer from '../factories/createNPCsFromLayer'
import { ILevelComponents } from '../ILevel'

/*
 * This is a function to add npcs to a map from a layer
 */
export default function(level: ILevelComponents, layer: string): void {
    const npcs = createNPCsFromLayer(layer, level)
    for (const npc of npcs) {
        level.scene.add.existing(npc[0])
        level.npcs.add(npc[0])
        level.machineRegisty.add(npc[0].name + '_machine', interpret(npc[1]))
    }
    level.machineRegisty.startAll()
}
