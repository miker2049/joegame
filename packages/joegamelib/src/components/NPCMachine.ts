import {
  assign,
  send,
  spawn,
  StateMachine,
  createMachine,
  ActorRef,
  sendTo
} from 'xstate'
import { Dir } from '../joegameTypes'
import {
  MoveMachineEvent,
  createMoveMachine,
  IMachineCharacter
} from './MoveMachine'
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
  // patience: ,
  // ActorRefnumber
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
  moveMachineRef?: ActorRef<MoveMachineEvent>
  character: IMachineCharacter
  tileSize: number
  finder: IPathfinder
}

interface IBumper {
  name: string
  body: Phaser.Physics.Arcade.Body
  x: number
  y: number
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

const charMachine = (name: string) =>
  createMachine<NPCContext, NPCEvent>(
    {
      predictableActionArguments: true,
      id: 'NPCMachine' + name,
      initial: 'idle',
      entry: 'spawnMoveMachine',
      on: {
        // AUTO_OFF:{actions: assign({auto: ()=>false})},
        AUTO_OFF: {
          actions: assign({ auto: (_) => false }),
          target: 'idle'
        },
        AUTO_ON: {
          actions: assign({ auto: (_) => true }),
          target: 'idle'
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
              actions: () => {
                console.log('you say something')
              }
            },
            BUMP: {
              target: 'stoppedAndTurned'
            }
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
              actions: () => {
                console.log('you say something')
              }
            },
            BUMP: {
              target: 'stoppedAndTurned'
            }
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
    },
    {
      actions: {
        chooseInterest: assign({
          interestCounter: (context) => {
            return (context.interestCounter + 1) % context.interests.length
          },
          currentDestination: (context) => {
            const curr = context.interests[context.interestCounter]
            return { x: curr.x, y: curr.y }
          }
        }),
        setInterest: assign({
          currentDestination: (context, event) => {
            if (event.type === 'MOVE_THOUGHT') return { x: event.x, y: event.y }
            else return context.currentDestination
          },
          tmpFinalFacing: (_context, event) => {
            if (
              event.type === 'MOVE_THOUGHT' &&
              event.finalFacing != undefined
            ) {
              return event.finalFacing
            } else {
              return undefined
            }
          }
        }),
        unsetFinalFacing: assign<NPCContext, any>({
          tmpFinalFacing: (_context) => undefined
        }),
        startDialogue: (_context) => {},
        reactToCollider: assign({
          additionalAvoid: (
            context,
            event: { type: 'BUMP'; sprite: IBumper }
          ) => {
            const out = getTileFromBody(event.sprite, context.tileSize)
            if (!out.x || !out.y) return { x: 0, y: 0 }
            else return out
          }
        }),
        jumpBack: sendTo(
          (context) => context.moveMachineRef as ActorRef<MoveMachineEvent>,
          (context, event: { type: 'BUMP'; sprite: IBumper }) => {
            const collider = event.sprite
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
          }
        ),
        exclaim: (_context) => {},
        announceReached: (_context) => {},
        moveToInterest: sendTo(
          (context) => context.moveMachineRef as ActorRef<MoveMachineEvent>,
          (context) => {
            return {
              type: 'MOVE_ON_PATH',
              point: {
                x: context.currentDestination.x,
                y: context.currentDestination.y
              },
              tempObs: {
                x: context.additionalAvoid.x,
                y: context.additionalAvoid.y
              }
            }
          }
        ),
        spawnMoveMachine: assign({
          moveMachineRef: (context) => {
            return spawn(
              createMoveMachine(
                context.character,
                context.tileSize,
                context.finder
              )
            )
          }
        })
        // duration:2000,
      },
      guards: {
        isAuto: (context) => context.auto
      },
      activities: {},
      services: {},
      delays: {}
    }
  )

export function createNPCMachine(
  character: IMachineCharacter,
  tileSize: number,
  finder: IPathfinder,
  interests: Array<{ x: number; y: number; finalFacing?: Dir }>
) {
  return charMachine(character.name).withContext({
    character,
    tileSize,
    finder,
    interests,
    currentDestination: interests[1],
    // patience: defaults.patience,
    auto: true,
    tmpFinalFacing: undefined,
    interestCounter: 0,
    additionalAvoid: { x: 0, y: 0 }
  })
}
