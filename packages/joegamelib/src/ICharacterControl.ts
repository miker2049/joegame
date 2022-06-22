import 'phaser'
import { Interpreter } from 'xstate'
import easystar from 'easystarjs';

enum Dir {
	north="north",
	south="south",
	east="east",
	west="west"
}

export interface ICharacterControl {
    body: Phaser.Types.Physics.Arcade.GameObjectWithDynamicBody
    changeGroundVel(vel: Phaser.Types.Math.Vector2Like): void
    move(dir: Dir): void
    dash(dir: Dir): void
    face(dir: Dir): void
    stop(face?: Dir): void
    playAnim(anim: string): void
    moveToPoint(point: Phaser.Types.Math.Vector2Like, finder: easystar.js): void
}

export interface ICharacterUIControl {
    toggleDashBoxes(): void
    minusDashBox(): void
    showLabel(): void
    hideLabel(): void
    speak(msg:string): void
}

export interface ICharacterSettings {
    speed: number
    name: string
    dashDistance: number
    charge: number
}

export interface ICharacterState {
    facing: Dir
    onPlatform: boolean
    auto: boolean
    player: boolean
    groundVel: Phaser.Types.Math.Vector2Like
}

export interface ICharacterMoveMachine{
    send(msg:string): void
    start(): void
    pause(): void
    kill(): void
    moveMachine: Interpreter<any>
}

export interface ICharacterNPCMachine{
    start(): void
    pause(): void
    kill(): void
    moveMachine: Interpreter<any>
}

export interface ICharacter extends ICharacterControl, ICharacterUIControl, ICharacterSettings, ICharacterState, ICharacterMoveMachine {}
export interface INPC extends ICharacter, ICharacterNPCMachine {}

