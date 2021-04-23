import { interpret } from 'xstate';
import createNPCsFromLayer from '../factories/createNPCsFromLayer';
/*
 * This is a function to add npcs to a map from a layer
 */
export default function (level, layer) {
    const npcs = createNPCsFromLayer(layer, level);
    for (const npc of npcs) {
        level.scene.add.existing(npc[0]);
        level.npcs.add(npc[0]);
        level.machineRegistry.add(npc[0].name, interpret(npc[1]));
    }
    level.machineRegistry.startAll();
}
//# sourceMappingURL=addAllNPCsFromLayer.js.map