import { Runner, BondageResults } from "bondage";
import { IMachineRegistry } from './MachineRegistry';
export default class DialogueReader {
    private registry;
    runner: Runner;
    allNodes: string[];
    currRun: Generator<BondageResults, BondageResults, undefined> | undefined;
    mapTileSize: number;
    constructor(scene: Phaser.Scene, yarnjson: object, registry: IMachineRegistry, tilesize: number);
    private registerCommands;
    getRunner(node: string): Generator<BondageResults, BondageResults, undefined>;
}
//# sourceMappingURL=DialogueReader.d.ts.map