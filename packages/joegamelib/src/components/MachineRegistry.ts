import {
  InterpreterStatus,
  Interpreter,
  AnyInterpreter,
  AnyStateMachine,
  interpret,
  AnyEventObject
} from 'xstate'

export type Machine<T> = Interpreter<T>

export class MachineRegistry {
  machines: Map<string, Machine<AnyInterpreter>>
  constructor() {
    this.machines = new Map()
  }
  add(char: string, mach: AnyStateMachine): void {
    this.machines.set(char, interpret(mach))
  }
  startAll(): void {
    this.machines.forEach((mach) => mach.start())
  }
  stopAll(): void {
    this.machines.forEach((mach) => mach.stop())
  }

  sendTo(char: string, event: AnyEventObject): void {
    const charm = this.machines.get(char)
    if (charm != undefined) {
      charm.send(event)
    } else {
      console.log(`There is not ${char} machine in the registry`)
    }
  }
  checkStatus(mach: string): InterpreterStatus | undefined {
    const status = this.machines.get(mach)?.status
    if (!status) return undefined
    else return status
  }
  entries() {
    return this.machines.entries()
  }
  get(k: string) {
    return this.machines.get(k)
  }
}
