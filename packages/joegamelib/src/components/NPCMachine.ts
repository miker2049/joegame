import { assign, spawn, createMachine, ActorRef, sendTo } from 'xstate'
import { Dir } from '../joegameTypes'
import { MoveMachineEvent, createMoveMachine } from './MoveMachine'
import { IPathfinder } from '../ILevel'
import getTileFromBody from '../utils/getTileFromBody'
import Character from '../Character'
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
  character: Character
  tileSize: number
  finder: IPathfinder
  /**
   * How long a character waits.
   */
  patience: number
  /**
   * Extra wait time for the first time
   */
  startOffset: number
  firstWait: boolean
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
  | { type: 'SPEAK_THOUGHT'; text: string; convoId?: string }
  | { type: 'NO_PATH' }
  | { type: 'DUMMY' }
  | { type: 'WALL_BUMP' }
  | { type: 'CONVERSATION_DONE' }

const charMachine = (name: string) =>
  createMachine<NPCContext, NPCEvent>(
    {
      predictableActionArguments: true,
      id: 'NPCMachine_' + name,
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
            NO_PATH: {
              target: 'idle',
              actions: 'removeInterest'
            },
            SPEAK_THOUGHT: {
              target: 'speaking'
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
            1000: [{ target: 'going' }]
          },
          on: {
            TALK_TO: {
              target: 'conversing',
              actions: 'startDialogue'
            },
            BUMP: {
              target: 'stoppedAndTurned'
            }
          }
        },
        idle: {
          on: {
            MOVE_THOUGHT: {
              target: 'going',
              actions: ['setInterest'],
              cond: 'hasCurrentDestination'
            },
            NO_PATH: {
              actions: ['removeInterest']
            },
            SPEAK_THOUGHT: {
              target: 'speaking'
            },
            BUMP: {
              target: 'stoppedAndTurned'
            }
          },
          after: {
            PATIENCE: {
              target: 'going',
              cond: 'isAuto',
              actions: ['chooseInterest']
            }
          }
        },
        speaking: {
          invoke: {
            id: 'speaking_' + name,
            src: async (context, event) => {
              if (event.type === 'SPEAK_THOUGHT') {
                await context.character.speak(event.text)
                return event.convoId || undefined
              } else return undefined
            },
            onDone: {
              target: 'idle',
              actions: (context, event) => {
                if (event.data)
                  context.character.scene.machineRegistry.sendTo(event.data, {
                    type: 'FINISHED_TALKING'
                  })
              }
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
            return context.interests.length === 0
              ? context.interestCounter
              : (context.interestCounter + 1) % context.interests.length
          },
          currentDestination: (context) => {
            if (context.interests.length === 0) {
              return context.currentDestination
            } else {
              const curr =
                context.interests[
                  context.interestCounter % context.interests.length
                ]
              return { x: curr.x, y: curr.y }
            }
          },
          firstWait: () => false
        }),
        removeInterest: assign({
          interests: (c) => {
            if (c.interests.length === 1) {
              return c.interests
            } else {
              const inter = [...c.interests]
              const i = inter.findIndex(
                (v) =>
                  v.x === c.currentDestination.x &&
                  v.y === c.currentDestination.y
              )
              if (i) inter.splice(i, 1)
              return inter
            }
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
            return out
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
            if (context.currentDestination)
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
            else return { type: 'DUMMY' }
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
        isAuto: (context) => context.auto,
        hasCurrentDestination: (context) => !!context.currentDestination
      },
      activities: {},
      services: {},
      delays: {
        PATIENCE: (context, _ev) => {
          if (context.firstWait) {
            return context.patience + context.startOffset
          } else {
            return context.patience
          }
        }
      }
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
    additionalAvoid: { x: 0, y: 0 },
    patience: Math.random() * 15000 + 3000,
    startOffset: Math.random() * 3000,
    firstWait: true
  })
}
