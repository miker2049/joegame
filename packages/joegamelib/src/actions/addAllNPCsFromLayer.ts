import defaults from 'defaults'
import { interpret } from 'xstate'
import createNPCsFromLayer from '../factories/createNPCsFromLayer'
import { ILevelComponents } from '../ILevel'

/*
 * This is a function to add npcs to a map from a layer
 */
export default function(level: ILevelComponents, layer: string): void {
    const depth=level.scene.game.registry.get('depthmap').get(layer) ?? defaults.platformDepth
    const npcs = createNPCsFromLayer(layer, level, depth)

    for (const npc of npcs) {
        level.scene.add.existing(npc[0])
        level.npcs.add(npc[0])
        level.machineRegistry.add(npc[0].name, interpret(npc[1]))
    }
    level.machineRegistry.startAll()
}
