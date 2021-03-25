import { InterpreterStatus } from 'xstate';
export class MachineRegistry {
    constructor() {
        this.machines = new Map();
    }
    add(char, mach) {
        this.machines.set(char, mach);
    }
    startAll() {
        this.machines.forEach((mach) => mach.start());
    }
    stopAll() {
        this.machines.forEach((mach) => mach.stop());
    }
    sendTo(char, event) {
        const charm = this.machines.get(char);
        if (charm != undefined) {
            charm.send(event);
        }
        else {
            console.log(`There is not ${char} machine in the registry`);
        }
    }
    checkStatus(mach) {
        var _a;
        return ((_a = this.machines.get(mach)) === null || _a === void 0 ? void 0 : _a.status) || InterpreterStatus.NotStarted;
    }
}
//# sourceMappingURL=MachineRegistry.js.map