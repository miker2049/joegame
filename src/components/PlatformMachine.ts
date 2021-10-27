import { createMachine } from "xstate/lib/Machine";
import { MachineConfig, MachineOptions, StateMachine } from "xstate/lib/types";
import Platform from "./Platform";

interface PlatformMachineContext {
    locations: {x: number, y: number}[]
    currIndex: number
    delay: number
    thisPlatform: Platform
    speed: number
    auto: boolean
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
            const ind = context.currIndex+1 % context.locations.length
            const nextLocation = context.locations[ind]
            context.thisPlatform.scene.physics.accelerateTo(
                context.thisPlatform,
                nextLocation.x,
                nextLocation.y,
                context.speed,
                context.speed,
                context.speed
            )
            // context.thisPlatform.body.
        },
    },
    guards: {
        isAuto: (context, _) => context.auto
    },
    activities: {
    },
    services: {
    },
    delays: {
        WAIT_DELAY: (context, _) => {
            return context.delay
        },
    }
}

function createPlatformMachine(config: PlatformMachineConfig): StateMachine<PlatformMachineContext, any, any> {
    const opts = PlatformMachineOptions
    const mconf: MachineConfig<PlatformMachineContext, any, any> = {
        key: config.name,
        initial: 'still',
        context: {
            locations: config.locations,
            auto: config.auto,
            speed: config.speed,
            delay: config.delay,
            currIndex: config.currIndex,
            thisPlatform: config.thisPlatform,
        },
        states: {
            still: {
                entry: 'stillAction',
                on: {
                    MOVE: 'moving'
                }
            },
            moving: {
                on: {
                    DESTINATION_REACHED: 'still'
                }
            }
        }
    }
    return createMachine(mconf, opts)
}
