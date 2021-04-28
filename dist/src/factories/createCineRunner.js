import { __awaiter } from "tslib";
import 'phaser';
import bondage from 'bondage';
const Runner = bondage.Runner;
export default function (level, yarnjson, textWindow) {
    const runner = new Runner();
    runner.load(yarnjson);
    runner.setVariableStorage(level.scene.registry);
    //NOTE assuming square tile
    const tileSize = level.map.tileWidth;
    runner.registerFunction('wait', (args) => __awaiter(this, void 0, void 0, function* () {
        yield new Promise(resolve => { setTimeout(resolve, args[0]); });
    }));
    runner.registerFunction('moveChar', (args) => {
        level.machineRegistry.sendTo(args[0], { type: 'MOVE_ON_PATH', point: { x: args[1] * tileSize, y: args[2] * tileSize } });
    });
    runner.registerFunction('moveCharSync', (args) => {
        level.machineRegistry.sendTo(args[0], { type: 'MOVE_ON_PATH', point: { x: args[1] * tileSize, y: args[2] * tileSize } });
        return new Promise(resolve => {
            level.machineRegistry.machines.get(args[0]).onTransition((state) => {
                if (state.value === 'still') {
                    resolve(null);
                }
            });
        });
    });
    runner.registerFunction('transportChar', (args) => {
        level.machineRegistry.sendTo(args[0], { type: 'TRANSPORT', point: { x: args[1] * tileSize, y: args[2] * tileSize } });
    });
    runner.registerFunction('openWindow', (_args) => {
        textWindow.open();
    });
    runner.registerFunction('closeWindow', (_args) => {
        textWindow.close();
    });
    runner.registerFunction('clearWindowText', (_args) => {
        textWindow.setMDText('');
    });
    return runner;
}
//# sourceMappingURL=createCineRunner.js.map