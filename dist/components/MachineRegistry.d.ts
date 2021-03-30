import { InterpreterStatus } from 'xstate';
export interface IMachine {
    send(event: any): any;
    start(): void;
    stop(): void;
    onTransition(func: Function): void;
    status: InterpreterStatus;
}
export interface IMachineRegistry {
    machines: Map<string, IMachine>;
    startAll(): void;
    stopAll(): void;
    sendTo(char: string, event: any): void;
    checkStatus(mach: string): InterpreterStatus;
    add(char: string, mach: IMachine): void;
}
export declare class MachineRegistry implements IMachineRegistry {
    machines: Map<string, IMachine>;
    constructor();
    add(char: string, mach: IMachine): void;
    startAll(): void;
    stopAll(): void;
    sendTo(char: string, event: any): void;
    checkStatus(mach: string): InterpreterStatus;
}
//# sourceMappingURL=MachineRegistry.d.ts.map