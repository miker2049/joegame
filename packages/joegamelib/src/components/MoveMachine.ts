import { AssuredVec2, Dir } from '../joegameTypes'
import { invoke, createMachine, state, reduce, transition } from 'robot3'
import 'phaser'
import getPath from '../utils/getPath';
import { IPathfinder } from '../ILevel';
import getDirFromTransition from '../utils/getDirFromTransition';
import moveDistance from '../actions/moveDistance';
import { ICharacterMoveMachine } from '../ICharacter';
import getTileFromPoint from '../utils/getTileFromPoint';

/*
 * A machine character is somethig that has:
 */
export interface IMachineCharacter {
    move(dir: Dir): void
    dash(dir: Dir): void
    transport(x: number, y: number): void
    charge: number
    //for calculating dash length
    dashVel: number
    dashDrag: number
    minusCharge(): void
    jumpUp(): void
    jumpBack(dir: Dir): void
    stop(): void
    changeGroundVel(vel: AssuredVec2): void
    name: string
    align(): AssuredVec2
    x: number
    y: number
    speed: number
    player: boolean
    auto: boolean
    facing: Dir
}

export interface MoveMachineContext {
    char: IMachineCharacter
    finder: IPathfinder
    tileSize: number
}

export type MoveMachineEvent =
    | { type: "MOVE", dir: Dir }
    | { type: "DASH", dir: Dir }
    | { type: "PLATFORM_CHANGE"; vel: { x: number; y: number } }
    | { type: "MOVE_ON_PATH"; point: { x: number; y: number }; tempObs?: { x: number; y: number } }
    | { type: "BUMP"; dir: Dir }
    | { type: "LEAVE_PLATFORM" }
    | { type: "DESTINATION_REACHED" }
    | { type: "NO_PATH" }
    | { type: "STOP" }
    | { type: "TRANSPORT"; point: { x: number; y: number } }

interface MoveMachineStateSchema {
    states: {
        dashing: {}
        moving: {}
        onPath: {}
        still: {}
        dead: {}
    };
}
const MoveMachine = createMachine({
    dashing: state(),
    moving: state(),
    onPath: state(),
    still: state(),
    dead: state(),
})
interface IPathmoveMachineContext {
    tileSize: number
    char: IMachineCharacter & ICharacterMoveMachine
    destTile: AssuredVec2
    tempObsTile: AssuredVec2
    finder: IPathfinder
    path: AssuredVec2[]
    doneState: string
    errorState: string
}


const invokeGetPath = ()=>
    invoke((context: IPathmoveMachineContext) => {
    const charPlace = context.char.align()
    return getPath({
        x: charPlace.x,
        y: charPlace.y,
        dx: context.destTile.x,
        dy: context.destTile.y,
        tempObsX: context.tempObsTile.x,
        tempObsY: context.tempObsTile.y,
        finder: context.finder
    })
}, transition('done', 'doneGetPath',
        reduce((ctx: any, ev: any) => { return { ...ctx, path: ev.data } })),
    transition('error','errorGetPath'))

const PathmoveMachine = createMachine({
    gettingpath: invokeGetPath(),
    movingOnPath: {
        invoke: {
            src: async (context) => {
                if (context.path.length > 0) {
                    const direction = getDirFromTransition(context.path[0] as { x: number, y: number })
                    return moveDistance({
                        gobject: context.char,
                        dir: direction,
                        distance: context.tileSize,
                        stop: false
                    })
                } else {
                    console.log('no path!')
                }
            },
            onDone: [
                {
                    target: 'movingOnPath',
                    actions: assign({
                        path: (context) => {
                            // console.log(context.path);
                            if (context.path.length > 1) {
                                return context.path.slice(1)
                            } else {
                                return []
                            }
                        }
                    }),
                    cond: (context) => context.path.length > 1
                },
                { target: 'reachedDestination' }

            ]
        }
    },
    reachedDestination: {
        type: 'final'
    }
})

export const MoveMachineConfig = (
    character: IMachineCharacter,
    tileSize: number,
    finder: IPathfinder,
): MachineConfig<MoveMachineContext, MoveMachineStateSchema, MoveMachineEvent> => {
    return {
        key: `${character.name}_movemachine`,
        initial: 'still',
        entry: ['setGroundVel'],
        context: {
            char: character,
            tileSize: tileSize,
            finder: finder,
        },
        on: {
            TRANSPORT: {
                target: 'still',
                actions: 'transport'
            }
        },
        states: {
            still: {
                entry: ['stillAction'],
                on: {
                    MOVE: ['moving'],
                    MOVE_ON_PATH: { target: "onPath" },
                    DASH: [
                        { target: 'dashing', cond: 'hasCharge' },
                        { actions: 'noChargeAction' },
                    ],
                    STOP: { target: 'still', actions: 'setGroundVel' },
                    PLATFORM_CHANGE: { target: 'still', actions: 'setGroundVel' },
                    BUMP: { target: 'still', actions: 'jumpBack' }
                }
            },
            onPath: {
                invoke: {
                    src: (context) => createPathmoveMachine(context.char.name),
                    data: {
                        tileSize: (context: MoveMachineContext) => context.tileSize,
                        char: (context: MoveMachineContext) => context.char,
                        destTile: (context: MoveMachineContext, event: { type: string, point: { x: number, y: number } }) => {
                            const tile = getTileFromPoint({ x: event.point.x, y: event.point.y }, context.tileSize)
                            return { x: tile.x, y: tile.y }
                        },
                        tempObsTile: (_context: MoveMachineContext, event) => event.tempObs || { x: -9999, y: -9999 },
                        finder: (context: MoveMachineContext) => context.finder,
                        path: []
                    },
                    onDone: { target: 'still', actions: sendParent('DESTINATION_REACHED') }
                },
                on: {
                    DESTINATION_REACHED: 'still',
                    BUMP: { actions: 'jumpBack', target: 'still' },
                    MOVE_ON_PATH: { target: 'onPath' },
                    NO_PATH: { target: 'still', actions: 'jumpUp' },
                    MOVE: ['moving'],
                    DASH: [
                        { target: 'dashing', cond: 'hasCharge' },
                        { actions: 'noChargeAction' },
                    ],
                }
            },
            moving: {
                entry: ['movingAction'],
                on: {
                    MOVE: 'moving',
                    STOP: { target: 'still', actions: 'setGroundVel' },
                    BUMP: { actions: 'jumpBack', target: 'moving' },
                    LEAVE_PLATFORM: { target: 'moving', actions: 'setGroundVel' },
                    PLATFORM_CHANGE: { target: 'moving', actions: 'setGroundVel' }
                }
            },
            dashing: {
                entry: ['dashAction', 'minusCharge'],
                on: {
                    DASH: [
                        { target: 'dashing', cond: 'hasCharge' },
                        { actions: 'noChargeAction' },
                    ],
                },
                after: {
                    DASH_DELAY: { target: 'still', actions: 'setGroundVel' },
                }
            },
            dead: {
                type: 'final'
            }
        },
    }
};


const MoveMachineOptions = (): MachineOptions<MoveMachineContext, any> => {
    return {
        actions: {
            movingAction: (context, event) => {
                const dir = event.dir ? event.dir : context.char.facing
                context.char.move(dir)
            },
            dashAction: (context, event) => context.char.dash(event.dir),
            minusCharge: (context) => context.char.minusCharge(),
            jumpUp: (context) => context.char.jumpUp(),
            jumpBack: (context, event) => context.char.jumpBack(event.dir),
            setGroundVel: (context) => { context.char.changeGroundVel({ x: 0, y: 0 }) },
            stillAction: (context) => context.char.stop(),
            transport: (context, event) => context.char.transport(event.point.x, event.point.y)
        },
        guards: {
            hasCharge: (context, _event) => context.char.charge > 0 ? true : false,
        },
        activities: {
        },
        services: {
        },
        delays: {
            DASH_DELAY: (context, event) => {
                return (context.char.dashVel / context.char.dashDrag) * 1000
            },
        }
    }
}

export function createMoveMachine(char: IMachineCharacter, tileSize: number, finder: IPathfinder) {
    const config = MoveMachineConfig(char, tileSize, finder);
    const options = MoveMachineOptions();
    return createMachine<any, any, any>(config, options)
}
