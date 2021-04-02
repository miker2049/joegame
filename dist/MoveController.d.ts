import 'phaser';
import { Interpreter } from 'xstate';
import { MoveMachineContext } from './components/MoveMachine';
export default class MoveController {
    private moveMachine;
    private scene;
    held: Set<string>;
    constructor(moveMachine: Interpreter<MoveMachineContext>, scene: Phaser.Scene);
    setGameplayControl(): void;
    gameplayKeyDown: (event: KeyboardEvent) => void;
    gameplayKeyUp: (event: KeyboardEvent) => void;
    gameplayMouseDown: () => void;
    mapping: {
        ArrowLeft: () => void;
        ArrowRight: () => void;
        ArrowUp: () => void;
        ArrowDown: () => void;
        a: () => void;
        y: () => void;
        u: () => void;
        ' ': () => void;
    };
    shiftMapping: {
        ArrowLeft: () => void;
        ArrowRight: () => void;
        ArrowUp: () => void;
        ArrowDown: () => void;
        a: () => void;
        d: () => void;
        w: () => void;
        s: () => void;
        t: () => void;
        ' ': () => void;
    };
}
