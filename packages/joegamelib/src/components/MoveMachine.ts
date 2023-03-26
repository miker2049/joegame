import { assign, sendParent, createMachine } from 'xstate'
import { Dir } from '../joegameTypes'
import 'phaser'
import getPath from '../utils/getPath'
import { IPathfinder } from '../ILevel'
import getDirFromTransition from '../utils/getDirFromTransition'
import moveDistance from '../actions/moveDistance'
import { ICharacterMoveMachine } from '../ICharacter'
import getTileFromPoint from '../utils/getTileFromPoint'

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
  changeGroundVel(vel: Phaser.Types.Math.Vector2Like): void
  name: string
  align(): Phaser.Types.Math.Vector2Like
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
  | { type: 'TRANSPORT'; point: { x: number; y: number } }

interface IPathmoveMachineContext {
  tileSize: number
  char: IMachineCharacter & ICharacterMoveMachine
  destTile: Phaser.Types.Math.Vector2Like
  tempObsTile: Phaser.Types.Math.Vector2Like
  finder: IPathfinder
  path: Phaser.Types.Math.Vector2Like[]
}

const createPathmoveMachine = (name: string) =>
  createMachine<IPathmoveMachineContext>({
    id: `${name}s_pathmover`,
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
      reachedDestination: {
        type: 'final'
      }
    }
  })

export const createMoveMachine = (
  character: IMachineCharacter,
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
            }
          },
          on: {
            DESTINATION_REACHED: 'still',
            BUMP: { actions: 'jumpBack', target: 'still' },
            MOVE_ON_PATH: { target: 'onPath' },
            NO_PATH: { target: 'still', actions: 'jumpUp' },
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
        setGroundVel: (context) => {
          context.char.changeGroundVel({ x: 0, y: 0 })
        },
        stillAction: (context) => context.char.stop(),
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
