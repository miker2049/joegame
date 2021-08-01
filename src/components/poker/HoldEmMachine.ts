
import { Machine, assign, sendParent, send, MachineConfig, MachineOptions, SpawnedActorRef, spawn, StateMachine } from 'xstate'


interface HoldEmContext {
    playerNum: number
}

type HoldEmEvents = {type: "START"}

const hmachineConfig = (name: string): MachineConfig<HoldEmContext, any, HoldEmEvents> => {

    return {
        initial: "init",
        states: {
            init: {},
            pre_deal: {},
            pre_flop: {},
            flop: {},
            turn: {},
            river: {},
        }
    }

}
