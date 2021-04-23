import { createNPCMachine } from '../components/NPCMachine';
import createCharacter from '../factories/createCharacter';
export default function (name, interestSets, level) {
    const char = createCharacter(name, interestSets[0].x, interestSets[0].y, level);
    const mach = createNPCMachine(char, level.map.tileWidth, level.pathfinder, interestSets);
    return [char, mach];
}
//# sourceMappingURL=createNPC.js.map