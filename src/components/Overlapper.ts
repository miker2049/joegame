import { ILevelComponents } from "ILevel";
import "phaser";
import { interpret, Interpreter, Machine, MachineConfig } from "xstate";
import { ITiledMapObject, MapObject } from "./MapObject";
import {GameObjectInWorld} from '../joegameTypes'
import { ICharacter } from "ICharacter";

type GameObjectWithDynamicBody = Phaser.Types.Physics.Arcade.GameObjectWithDynamicBody
export type OverlapperPressEvent = { type: "PRESS", char: ICharacter}
export type OverlapMachineEvents = |
    OverlapperPressEvent |
{ type: "RELEASE" } |
{ type: "OFF" } |
{ type: "ON" }

export interface IOverlapper {
    pressedCallback: (context, event: OverlapperPressEvent) => void
    releasedCallback: (
        context,
        event: OverlapperPressEvent) => void
    active: boolean
    overlapMachine: Interpreter<{}, any,
        OverlapMachineEvents>
}

export class Overlapper extends MapObject implements IOverlapper {
    active: boolean
    overlapMachine: Interpreter<{}, any,
        OverlapMachineEvents>
    constructor(level: ILevelComponents, x: number, y: number, t_object: ITiledMapObject) {
        super(level, x, y, t_object)
        this.active = true

        this.overlapMachine = interpret(Machine(OverlapMachine()).withConfig({
            actions: {
                pressedAction: (context, event) => {
                    // event.type==OverlapperPressEvent ? this.pressedCallback(context, event)
                    if (event.type === 'PRESS') {
                        this.pressedCallback(context, event)
                    }
                },
                releasedAction: (context, event) => this.releasedCallback(context, event),
            }
        }))
    }

    pressedCallback(context, event: OverlapperPressEvent) { }
    releasedCallback(context, event) { }
}

export function OverlapMachine(): MachineConfig<{}, any, OverlapMachineEvents> {
    return {
        initial: "released",
        states: {
            pressed: {
                entry: "pressedAction",
                on: {
                    RELEASE: "released"
                }
            },
            released: {
                entry: "releasedAction",
                on: {
                    PRESS: "pressed"
                }
            },
        }
    }
}
