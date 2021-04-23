import { __awaiter } from "tslib";
import { Runner } from "bondage";
export default class DialogueReader {
    constructor(scene, yarnjson, registry, tilesize) {
        this.registry = registry;
        this.mapTileSize = tilesize;
        this.runner = new Runner();
        this.runner.load(yarnjson);
        this.runner.setVariableStorage(scene.registry);
        this.allNodes = [];
        for (const node in this.runner.yarnNodes) {
            this.allNodes.push(this.runner.yarnNodes[node].title);
        }
        this.registerCommands();
    }
    registerCommands() {
        this.runner.registerFunction('wait', (args) => __awaiter(this, void 0, void 0, function* () {
            yield new Promise(resolve => { setTimeout(resolve, args[0]); });
        }));
        this.runner.registerFunction('moveChar', (args) => {
            console.log(args[0], { type: 'MOVE_ON_PATH', point: { x: args[1] * this.mapTileSize, y: args[2] * this.mapTileSize } });
            this.registry.sendTo(args[0], { type: 'MOVE_ON_PATH', point: { x: args[1] * this.mapTileSize, y: args[2] * this.mapTileSize } });
        });
        this.runner.registerFunction('moveCharSync', (args) => {
            console.log(args[0], { type: 'MOVE_ON_PATH', point: { x: args[1] * this.mapTileSize, y: args[2] * this.mapTileSize } });
            this.registry.sendTo(args[0], { type: 'MOVE_ON_PATH', point: { x: args[1] * this.mapTileSize, y: args[2] * this.mapTileSize } });
            return new Promise(resolve => {
                this.registry.machines.get(args[0]).onTransition(state => {
                    console.log(state);
                    if (state.value === 'still') {
                        resolve();
                    }
                });
            });
        });
        this.runner.registerFunction('transportChar', (args) => {
            this.registry.sendTo(args[0], { type: 'TRANSPORT', point: { x: args[1] * this.mapTileSize, y: args[2] * this.mapTileSize } });
        });
    }
    getRunner(node) {
        return this.runner.run(node);
    }
}
//# sourceMappingURL=DialogueReader.js.map