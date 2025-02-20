import 'phaser'
import { assign, createMachine, send, sendParent, sendTo } from 'xstate'
import moveDistance from '../actions/moveDistance'
import Character from '../Character'
import { IPathfinder } from '../ILevel'
import { Dir } from '../joegameTypes'
import getDirFromTransition from '../utils/getDirFromTransition'
import getPath from '../utils/getPath'
import getTileFromPoint from '../utils/getTileFromPoint'

export interface MoveMachineContext {
  char: Character
  finder: IPathfinder
  tileSize: number
}

export type MoveMachineEvent =
  | { type: 'MOVE'; dir: Dir }
  | { type: 'DASH'; dir: Dir }
  | { type: 'PLATFORM_CHANGE'; vel: { x: number; y: number } }
  | {
      type: 'MOVE_ON_PATH'
      point: { x: number; y: number }
      tempObs?: { x: number; y: number }
    }
  | { type: 'BUMP'; dir: Dir }
  | { type: 'LEAVE_PLATFORM' }
  | { type: 'DESTINATION_REACHED' }
  | { type: 'NO_PATH' }
  | { type: 'STOP' }
  | { type: 'DUMMY' }
  | { type: 'TRANSPORT'; point: { x: number; y: number } }

interface IPathmoveMachineContext {
  tileSize: number
  char: Character
  destTile: Phaser.Types.Math.Vector2Like
  tempObsTile: Phaser.Types.Math.Vector2Like
  finder: IPathfinder
  path: Phaser.Types.Math.Vector2Like[]
}

const createPathmoveMachine = (name: string) =>
  createMachine<IPathmoveMachineContext>({
    predictableActionArguments: true,
    // id: `${name}_pathmover`,
    initial: 'gettingpath',
    states: {
      gettingpath: {
        invoke: {
          src: (context, _event) => {
            const charPlace = context.char.align()
            return getPath({
              x: charPlace.x as number,
              y: charPlace.y as number,
              dx: context.destTile.x as number,
              dy: context.destTile.y as number,
              tempObsX: context.tempObsTile.x as number,
              tempObsY: context.tempObsTile.y as number,
              finder: context.finder
            })
          },
          onError: {
            actions: sendParent('NO_PATH'),
            target: 'noPath'
          },
          onDone: {
            actions: assign({
              path: (_context, event) => event.data
            }),
            target: 'movingOnPath'
          }
        }
      },
      movingOnPath: {
        invoke: {
          src: async (context) => {
            if (context.path.length > 0) {
              const direction = getDirFromTransition(
                context.path[0] as { x: number; y: number }
              )
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
      reachedDestination: { type: 'final' },
      noPath: { type: 'final' }
    }
  })

export const createMoveMachine = (
  character: Character,
  tileSize: number,
  finder: IPathfinder
) =>
  createMachine<MoveMachineContext, MoveMachineEvent>(
    {
      key: `${character.name}_movemachine`,
      initial: 'still',
      entry: ['setGroundVel'],
      context: {
        char: character,
        tileSize: tileSize,
        finder: finder
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
            MOVE_ON_PATH: { target: 'onPath' },
            DASH: [
              { target: 'dashing', cond: 'hasCharge' },
              { actions: 'noChargeAction' }
            ],
            STOP: { target: 'still', actions: 'setGroundVel' },
            PLATFORM_CHANGE: { target: 'still', actions: 'setGroundVel' },
            BUMP: { target: 'still', actions: 'jumpBack' }
          }
        },
        onPath: {
          invoke: {
            src: (context) => createPathmoveMachine(context.char.name),
            id: character.name + '_pathmover',
            data: {
              tileSize: (context: MoveMachineContext) => context.tileSize,
              char: (context: MoveMachineContext) => context.char,
              destTile: (
                context: MoveMachineContext,
                event: { type: string; point: { x: number; y: number } }
              ) => {
                const tile = getTileFromPoint(
                  { x: event.point.x, y: event.point.y },
                  context.tileSize
                )
                return { x: tile.x, y: tile.y }
              },
              tempObsTile: (
                _context: MoveMachineContext,
                event: MoveMachineEvent & {
                  type: 'MOVE_ON_PATH'
                }
              ) => event.tempObs || { x: -9999, y: -9999 },
              finder: (context: MoveMachineContext) => context.finder,
              path: []
            },
            onDone: {
              target: 'still',
              actions: sendParent('DESTINATION_REACHED')
            },
            onError: {
              target: 'still',
              actions: sendParent('NO_PATH')
            }
          },
          on: {
            DESTINATION_REACHED: 'still',
            BUMP: {
              actions: ['jumpBack'],
              target: 'still'
            },
            MOVE_ON_PATH: { target: 'onPath' },
            NO_PATH: {
              // target: 'still',
              actions: sendParent('NO_PATH')
            },
            MOVE: ['moving'],
            DASH: [
              { target: 'dashing', cond: 'hasCharge' },
              { actions: 'noChargeAction' }
            ]
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
              { actions: 'noChargeAction' }
            ]
          },
          after: {
            DASH_DELAY: { target: 'still', actions: 'setGroundVel' }
          }
        },
        dead: {
          type: 'final'
        }
      }
    },
    {
      actions: {
        movingAction: (context, event: MoveMachineEvent & { type: 'MOVE' }) => {
          const dir = event.dir ? event.dir : context.char.facing
          context.char.move(dir)
        },
        dashAction: (context, event: MoveMachineEvent & { type: 'DASH' }) =>
          context.char.dash(event.dir),
        minusCharge: (context) => context.char.minusCharge(),
        jumpUp: (context) => context.char.jumpUp(),
        jumpBack: (context, event: MoveMachineEvent & { type: 'BUMP' }) =>
          context.char.jumpBack(event.dir),
        setGroundVel: (_context) => {
          // context.char.changeGroundVel({ x: 0, y: 0 })
        },
        stillAction: (context) => context.char.stopp(),
        removeDestination: (_context) => {
          console.log('carby')
          sendParent('NO_PATH')
        },
        transport: (context, event: MoveMachineEvent & { type: 'TRANSPORT' }) =>
          context.char.transport(event.point.x, event.point.y)
      },
      guards: {
        hasCharge: (context, _event) => (context.char.charge > 0 ? true : false)
      },
      activities: {},
      services: {},
      delays: {
        DASH_DELAY: (context) => {
          return (context.char.dashVel / context.char.dashDrag) * 1000
        }
      }
    }
  )
