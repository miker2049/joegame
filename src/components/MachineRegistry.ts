import { NPCEvent } from './NPCMachine'
import { MoveMachineEvent } from './MoveMachine'
import {Interpreter, InterpreterStatus} from 'xstate'

export interface IMachine {
    send(event: any): any
    start(): void
    stop(): void
    onTransition(func: Function): void
    status: InterpreterStatus
}
export interface IMachineRegistry {
    machines: Map<string, IMachine>
    startAll(): void
    stopAll(): void
    sendTo(char: string, event: any): void
    checkStatus(mach: string): InterpreterStatus
    add(char: string, mach: IMachine): void
}

export class MachineRegistry implements IMachineRegistry {
    machines: Map<string, IMachine>
    constructor() {
        this.machines = new Map<string, IMachine>()
    }
    add(char: string, mach: IMachine): void {
        this.machines.set(char, mach)
    }
    startAll(): void {
        this.machines.forEach((mach)=>mach.start())
    }
    stopAll(): void {
        this.machines.forEach((mach)=>mach.stop())
    }
    sendTo(char: string, event: any): void {
        const charm = this.machines.get(char)
        if(charm!=undefined){
            charm.send(event)
        } else {
            console.log(`There is not ${char} machine in the registry`)
        }
    }
    checkStatus(mach: string): InterpreterStatus {
        return this.machines.get(mach)?.status || InterpreterStatus.NotStarted
    }
}
