import 'phaser';
import createNPC from '../utils/createNPC';
export default function* (layer, level, depth) {
    if (!level.map.getObjectLayer(layer)) {
        return;
    }
    // first, create an object with keys that correspond to the npc names
    // that have values which are sets of locations in absolute pixels
    let interestSets = {};
    for (let obj_ of level.map.getObjectLayer(layer).objects) {
        if (interestSets[obj_.name]) {
            interestSets[obj_.name].push({ x: obj_.x, y: obj_.y, NPCtype: obj_.type });
        }
        else {
            interestSets[obj_.name] = [{ x: obj_.x, y: obj_.y, NPCtype: obj_.type }];
        }
    }
    // now we iterate through our new object and create the npcs
    for (let name in interestSets) {
        // create the npc
        console.log(interestSets);
        yield createNPC(name, interestSets[name], level);
    }
    ;
}
//# sourceMappingURL=createNPCsFromLayer.js.map