import { SpawnedActorRef, StateMachine } from 'xstate';
import { Dir } from '../joegameTypes';
import { MoveMachineEvent, IMachineCharacter } from './MoveMachine';
import { IPathfinder } from '../ILevel';
export interface NPCContext {
    currentDestination: {
        x: number;
        y: number;
        finalFacing?: Dir;
    };
    patience: number;
    additionalAvoid: {
        x: number;
        y: number;
    };
    interestCounter: number;
    interests: Array<{
        x: number;
        y: number;
        finalFacing?: Dir;
    }>;
    auto: boolean;
    tmpFinalFacing: string | undefined;
    moveMachineRef?: SpawnedActorRef<MoveMachineEvent>;
    character: IMachineCharacter;
    tileSize: number;
    finder: IPathfinder;
}
interface IBumper {
    name: string;
    body: Phaser.Physics.Arcade.Body;
}
export interface locationDirection {
    x: number;
    y: number;
    finalFacing: Dir;
}
export declare type NPCEvent = {
    type: 'AUTO_OFF';
} | {
    type: 'AUTO_ON';
} | {
    type: 'BUMP';
    sprite: IBumper;
} | {
    type: 'DESTINATION_REACHED';
} | {
    type: 'TALK_TO';
} | {
    type: 'MOVE_THOUGHT';
    x: number;
    y: number;
    finalFacing?: string;
} | {
    type: 'SPEAK_THOUGHT';
} | {
    type: 'NO_PATH';
} | {
    type: 'WALL_BUMP';
} | {
    type: 'CONVERSATION_DONE';
};
export declare function createNPCMachine(char: IMachineCharacter, tileSize: number, finder: IPathfinder, interests: Array<{
    x: number;
    y: number;
    finalFacing?: Dir;
}>): StateMachine<NPCContext, any, NPCEvent>;
export {};
