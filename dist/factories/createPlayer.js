import { interpret, Machine } from 'xstate';
import createCharacter from './createCharacter';
import { createMoveMachine } from '../components/MoveMachine';
import MoveController from '../MoveController';
export default function (name, x, y, level) {
    const char = createCharacter(name, x, y, level);
    const moveMachine = interpret(createMoveMachine(char, level.map.tileWidth, level.pathfinder), { devTools: true, parent: interpret(Machine({ id: name + 'dummyparent' })) });
    level.machineRegistry.add('player_machine', moveMachine);
    new MoveController(moveMachine, level.scene);
    return char;
}
//# sourceMappingURL=createPlayer.js.map