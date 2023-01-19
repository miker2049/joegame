import { actions } from 'xstate'
import { assign, send } from 'xstate/lib/actions'
import { createMachine } from 'xstate/lib/Machine'
import { MachineConfig, MachineOptions, StateMachine } from 'xstate/lib/types'
import Platform from './Platform'

interface PlatformMachineContext {
  locations: { x: number; y: number }[]
  currIndex: number
  delay: number
  thisPlatform: Platform
  speed: number
  auto: boolean
  currDistance: number
}

interface PlatformMachineConfig extends PlatformMachineContext {
  name: string
}

const PlatformMachineOptions: MachineOptions<PlatformMachineContext, any> = {
  actions: {
    stillAction: (context, _) => {
      context.thisPlatform.body.setAcceleration(0, 0)
      context.thisPlatform.body.setVelocity(0, 0)
      context.thisPlatform.notifyVelChange()
    },
    moveAction: (context, _) => {
      const nextLocation = context.locations[context.currIndex]
      context.thisPlatform.scene.physics.moveTo(
        context.thisPlatform,
        nextLocation.x,
        nextLocation.y,
        context.speed
        // context.speed,
      )
      context.thisPlatform.notifyVelChange()
      // context.thisPlatform.body.
    },
    getCurrDistance: assign({
      currDistance: (context, _) => {
        const nextLocation = context.locations[context.currIndex]
        const dist = Phaser.Math.Distance.BetweenPoints(
          context.thisPlatform,
          nextLocation
        )
        return dist
      }
    }),
    incCurrIndex: assign({
      currIndex: (context, _) => {
        return (context.currIndex + 1) % context.locations.length
      }
    }),
    delayMove: send({ type: 'MOVE' }, { delay: 1000 })
  },
  guards: {
    isAuto: (context, _) => context.auto
  },
  activities: {},
  services: {},
  delays: {
    WAIT_DELAY: (context, _) => {
      return context.delay
    },
    MOVE_DELAY: (context, _) => {
      return (context.currDistance / context.speed) * 1000
    }
  }
}

export function createPlatformMachine(
  config: PlatformMachineConfig
): StateMachine<PlatformMachineContext, any, any> {
  const opts = PlatformMachineOptions
  const mconf: MachineConfig<PlatformMachineContext, any, any> = {
    key: config.name,
    initial: 'still',
    context: {
      locations: config.locations,
      currDistance: 0,
      auto: config.auto,
      speed: config.speed,
      delay: config.delay,
      currIndex: config.currIndex + 1,
      thisPlatform: config.thisPlatform
    },
    states: {
      still: {
        entry: ['stillAction'],
        on: {
          MOVE: {
            target: 'moving',
            actions: 'getCurrDistance'
          },
          PRESSED: {
            actions: 'delayMove'
          }
        },
        after: {
          WAIT_DELAY: {
            target: 'moving',
            cond: 'isAuto',
            actions: 'getCurrDistance'
          }
        }
      },
      moving: {
        entry: ['moveAction'],
        after: {
          MOVE_DELAY: {
            target: 'still',
            actions: 'incCurrIndex'
          }
        }
      }
    }
  }
  return createMachine(mconf, opts)
}
