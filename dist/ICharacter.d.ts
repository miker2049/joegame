import 'phaser';
import { Interpreter } from 'xstate';
import { ILevelComponents } from './ILevel';
import { CharMoveAnims, Dir } from './joegameTypes';
import { MoveMachineContext } from './components/MoveMachine';
import { ITextBox } from './components/TextWindow';
export interface CharacterConfig {
    level: ILevelComponents;
    x: number;
    y: number;
    name: string;
    texture: string;
    anims: {
        north: string;
        south: string;
        east: string;
        west: string;
    };
    speed: number;
    scale: number;
    body?: {
        offsetY?: number;
        offsetX?: number;
        width?: number;
        height?: number;
    };
    dashDistance: number;
    depth?: number;
}
export interface IPathmover {
}
export interface ICharacterControl {
    charBody: Phaser.Physics.Arcade.Body;
    changeGroundVel(vel: Phaser.Types.Math.Vector2Like): void;
    move(dir: Dir): void;
    dash(dir: Dir): void;
    face(dir: Dir): void;
    stop(face?: Dir): void;
    align(): Phaser.Types.Math.Vector2Like;
    jumpUp(): void;
    jumpBack(dir: Dir): void;
    transport(x: number, y: number): void;
    transportNudge(x: number, y: number): void;
    playAnim(anim: string): void;
    minusCharge(): void;
}
export interface ICharacterUIControl {
    showDashboxes(): void;
    minusDashbox(): void;
    showLabel(): void;
    hideLabel(): void;
    speak(msg: string, speed?: number): Promise<any>;
    voxbox: ITextBox;
}
export interface ICharacterSettings {
    speed: number;
    name: string;
    dashDistance: number;
    charge: number;
    animKeys: CharMoveAnims;
}
export interface ICharacterPrivate {
    dashDrag: number;
    dashVel: number;
    sprite: Phaser.GameObjects.Sprite;
}
export interface ICharacterState {
    depth: number;
    facing: Dir;
    onPlatform: boolean;
    auto: boolean;
    player: boolean;
    groundVel: Phaser.Types.Math.Vector2Like;
}
export interface ICharacterMoveMachine {
    moveMachine: Interpreter<MoveMachineContext>;
}
export interface ICharacterNPCMachine {
    start(): void;
    pause(): void;
    kill(): void;
    npcMachine: Interpreter<any>;
}
export interface ICharacter extends ICharacterControl, ICharacterUIControl, ICharacterPrivate, ICharacterSettings, ICharacterState, Phaser.GameObjects.Container {
}
export interface INPC extends ICharacter, ICharacterNPCMachine {
}
export interface IPlayer extends ICharacter, ICharacterMoveMachine {
}
