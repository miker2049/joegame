import { Machine, assign, sendParent, send, MachineConfig, MachineOptions, SpawnedActorRef, spawn, StateMachine } from 'xstate'
import { Dir } from '../joegameTypes'
import { MoveMachineEvent, createMoveMachine, IMachineCharacter } from './MoveMachine'
import { IPathfinder } from '../ILevel'
import defaults from '../defaults'
import getTileFromBody from '../utils/getTileFromBody'
// - Machine
// - interpret
// - assign
// - send
// - sendParent
// - spawn
// - raise
// - actions
// - XState (all XState exports)




export interface NPCContext {
    currentDestination: {
        x: number
        y: number
        finalFacing?: Dir
    }
    patience: number
    additionalAvoid: {
        x: number
        y: number
    }
    interestCounter: number
    interests: Array<{
        x: number
        y: number
        finalFacing?: Dir
    }>
    auto: boolean
    tmpFinalFacing: string | undefined
    moveMachineRef?: SpawnedActorRef<MoveMachineEvent>
    character: IMachineCharacter
    tileSize: number
    finder: IPathfinder
}

interface IBumper {
    name: string
    body: Phaser.Physics.Arcade.Body
}

export interface locationDirection {
    x: number
    y: number
    finalFacing: Dir
}

export type NPCEvent =
    | { type: 'AUTO_OFF' }
    | { type: 'AUTO_ON' }
    | { type: 'BUMP'; sprite: IBumper }
    | { type: 'DESTINATION_REACHED' }
    | { type: 'TALK_TO' }
    | { type: 'MOVE_THOUGHT'; x: number; y: number; finalFacing?: string }
    | { type: 'SPEAK_THOUGHT' }
    | { type: 'NO_PATH' }
    | { type: 'WALL_BUMP' }
    | { type: 'CONVERSATION_DONE' }



const machineConfig = (name: string): MachineConfig<NPCContext, any, NPCEvent> => {
    return {

        id: 'NPCMachine' + name,
        initial: 'idle',
        entry: 'spawnMoveMachine',
        on: {
            // AUTO_OFF:{actions: assign({auto: ()=>false})},
            AUTO_OFF: {
                actions: assign({ auto: (context) => { return false } }),
                target: "idle"
            },
            AUTO_ON: {
                actions: assign({ auto: (context) => { return true } }),
                target: "idle"
            }
        },
        states: {
            going: {
                on: {
                    DESTINATION_REACHED: {
                        target: 'idle',
                        actions: ['unsetFinalFacing', 'announceReached']
                    },
                    SPEAK_THOUGHT: {
                        actions: () => { console.log('you say something') }
                    },
                    BUMP: {
                        target: 'stoppedAndTurned',
                    },
                },
                entry: ['moveToInterest']
            },
            stoppedAndTurned: {
                entry: ['reactToCollider', 'exclaim', 'jumpBack'],
                after: {
                    2000: [{ target: 'going' }]
                },
                on: {
                    TALK_TO: {
                        target: 'conversing',
                        actions: 'startDialogue'
                    }
                }
            },
            idle: {
                on: {
                    MOVE_THOUGHT: {
                        target: 'going',
                        actions: ['setInterest']
                    },
                    SPEAK_THOUGHT: {
                        actions: () => { console.log('you say something') }
                    },
                    BUMP: {
                        target: 'stoppedAndTurned',
                    },
                },
                after: {
                    2000: {
                        target: 'going',
                        cond: 'isAuto',
                        actions: ['chooseInterest']
                    }
                }
            },
            conversing: {
                on: {
                    CONVERSATION_DONE: 'idle'
                }
            }

        }
    }
}

// export default Machine(machineConfig,machineOptions)

const opts: MachineOptions<NPCContext, any> = {
    actions: {

        chooseInterest: assign({
            interestCounter: (context) => { return (context.interestCounter + 1) % context.interests.length },
            currentDestination: (context) => {
                const curr = context.interests[context.interestCounter]
                return { x: curr.x, y: curr.y }
            }
        }),
        setInterest: assign<any, Extract<{ type: 'MOVE_THOUGHT', x: number, y: number, finalFacing: string }, NPCEvent>>({
            currentDestination: (_context, event) => { return { x: event.x, y: event.y } },
            tmpFinalFacing: (_context, event) => { if (event.finalFacing != undefined) { return event.finalFacing } else { return undefined } }
        }),
        unsetFinalFacing: assign<NPCContext, any>({ tmpFinalFacing: (_context) => undefined }),
        startDialogue: (_context) => { },
        reactToCollider: assign<any, any>({
            additionalAvoid: (context, event) => getTileFromBody(event.sprite, context.tileSize)
        }),
        jumpBack: send((context, event) => {
            const collider = event.sprite
            console.log(event)
            if (collider.y > context.character.y) {
                return { type: 'BUMP', dir: Dir.south }
            } else if (collider.y < context.character.y) {
                return { type: 'BUMP', dir: Dir.north }
            } else if (collider.x < context.character.x) {
                return { type: 'BUMP', dir: Dir.west }
            } else if (collider.x > context.character.x) {
                return { type: 'BUMP', dir: Dir.east }
            } else {
                return { type: 'BUMP', dir: Dir.east }
            }
        }, {
            to: (context) => context.moveMachineRef as SpawnedActorRef<MoveMachineEvent>
        }),
        exclaim: (_context) => { },
        announceReached: (_context) => { },
        moveToInterest: send(
            (context) => {
                return {
                    type: 'MOVE_ON_PATH',
                    point: { x: context.currentDestination.x, y: context.currentDestination.y },
                    tempObs: { x: context.additionalAvoid.x, y: context.additionalAvoid.y }
                }
            },
            { to: (context) => context.moveMachineRef as SpawnedActorRef<MoveMachineEvent> }
        ),
        spawnMoveMachine: assign({
            moveMachineRef: (context) => {
                return spawn(createMoveMachine(context.character, context.tileSize, context.finder))
            }
        }),
        // duration:2000,
    },
    guards: {
        isAuto: (context) => context.auto
    },
    activities: {
    },
    services: {},
    delays: {}
}
export function createNPCMachine(char: IMachineCharacter,
    tileSize: number,
    finder: IPathfinder,
    interests: Array<{ x: number, y: number, finalFacing?: Dir }>
): StateMachine<NPCContext, any, NPCEvent> {
    return Machine<NPCContext, NPCEvent>(machineConfig(char.name), opts).withContext({
        character: char,
        tileSize: tileSize,
        finder: finder,
        interests: interests,
        currentDestination: interests[1],
        patience: defaults.patience,
        auto: true,
        tmpFinalFacing: undefined,
        interestCounter: 0,
        additionalAvoid: { x: 0, y: 0 }
    })
}
