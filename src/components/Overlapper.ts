import { ILevelComponents } from "ILevel";
import "phaser";
import { interpret, Interpreter, Machine, MachineConfig } from "xstate";
import { ITiledMapObject, MapObject } from "./MapObject";

type GameObjectWithDynamicBody = Phaser.Types.Physics.Arcade.GameObjectWithDynamicBody
export type OverlapMachineEvents = |
    { type: "PRESS", char: GameObjectWithDynamicBody } |
    { type: "RELEASE" } |
    { type: "OFF" } |
    { type: "ON" }

export interface IOverlapper {
    pressedCallback: () => void
    releasedCallback: () => void
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
                pressedAction: (context, event)=>this.pressedCallback(),
                releasedAction: (context, event)=>this.releasedCallback(),
            }
        }))
    }

    pressedCallback(){}
    releasedCallback(){}
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
