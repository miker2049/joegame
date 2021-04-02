import { MachineConfig, StateMachine } from 'xstate';
import { Dir } from '../joegameTypes';
import 'phaser';
import { IPathfinder } from '../ILevel';
export interface IMachineCharacter {
    move(dir: Dir): void;
    dash(dir: Dir): void;
    transport(x: number, y: number): void;
    charge: number;
    dashVel: number;
    dashDrag: number;
    minusCharge(): void;
    jumpUp(): void;
    jumpBack(dir: Dir): void;
    stop(): void;
    changeGroundVel(vel: Phaser.Types.Math.Vector2Like): void;
    name: string;
    align(): Phaser.Types.Math.Vector2Like;
    x: number;
    y: number;
    speed: number;
    player: boolean;
    auto: boolean;
    facing: Dir;
}
export interface MoveMachineContext {
    char: IMachineCharacter;
    finder: IPathfinder;
    tileSize: number;
}
export declare type MoveMachineEvent = {
    type: "MOVE";
    dir: Dir;
} | {
    type: "DASH";
    dir: Dir;
} | {
    type: "PLATFORM_CHANGE";
    vel: {
        x: number;
        y: number;
    };
} | {
    type: "MOVE_ON_PATH";
    point: {
        x: number;
        y: number;
    };
    tempObs?: {
        x: number;
        y: number;
    };
} | {
    type: "BUMP";
    dir: Dir;
} | {
    type: "LEAVE_PLATFORM";
} | {
    type: "DESTINATION_REACHED";
} | {
    type: "NO_PATH";
} | {
    type: "STOP";
} | {
    type: "TRANSPORT";
    point: {
        x: number;
        y: number;
    };
};
interface MoveMachineStateSchema {
    states: {
        dashing: {};
        moving: {};
        onPath: {};
        still: {};
        dead: {};
    };
}
export declare const MoveMachineConfig: (character: IMachineCharacter, tileSize: number, finder: IPathfinder) => MachineConfig<MoveMachineContext, MoveMachineStateSchema, MoveMachineEvent>;
export declare function createMoveMachine(char: IMachineCharacter, tileSize: number, finder: IPathfinder): StateMachine<MoveMachineContext, any, any, {
    value: any;
    context: MoveMachineContext;
}>;
export {};
