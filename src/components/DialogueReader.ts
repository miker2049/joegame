import { Runner, BondageResults, YarnNode } from "bondage";
import { IMachineRegistry } from './MachineRegistry';

export default class DialogueReader {

    runner: Runner
    allNodes: string[]
    currRun: Generator<BondageResults, BondageResults,undefined> | undefined
    mapTileSize: number


    constructor(scene: Phaser.Scene, yarnjson: object, private registry: IMachineRegistry, tilesize: number) {
        this.mapTileSize = tilesize
        this.runner = new Runner();
        this.runner.load(yarnjson as YarnNode[]);
        this.runner.setVariableStorage(scene.registry)
        this.allNodes = []
        for(const node in this.runner.yarnNodes){
           this.allNodes.push(this.runner.yarnNodes[node].title);
        }
        this.registerCommands();
    }

    private registerCommands() {
        this.runner.registerFunction('wait', async (args:[number]) => {
            await new Promise(resolve=>{setTimeout(resolve,args[0])})
        })
        this.runner.registerFunction('moveChar',(args: [string,number,number,string])=>{
            console.log(args[0], {type: 'MOVE_ON_PATH', point:{x: args[1]*this.mapTileSize, y: args[2]*this.mapTileSize}})
            this.registry.sendTo(args[0], {type: 'MOVE_ON_PATH', point:{x: args[1]*this.mapTileSize, y: args[2]*this.mapTileSize}})
        })
        this.runner.registerFunction('moveCharSync',(args: [string,number,number,string])=>{
            console.log(args[0], {type: 'MOVE_ON_PATH', point:{x: args[1]*this.mapTileSize, y: args[2]*this.mapTileSize}})
            this.registry.sendTo(args[0], {type: 'MOVE_ON_PATH', point:{x: args[1]*this.mapTileSize, y: args[2]*this.mapTileSize}})
            return new Promise(resolve=>{
                this.registry.machines.get(args[0])!.onTransition(state=>{
                    console.log(state)
                    if(state.value === 'still'){
                        resolve()
                    }
                })
            })
        })
        this.runner.registerFunction('transportChar',(args: [string,number,number,string])=>{
            this.registry.sendTo(args[0], {type: 'TRANSPORT', point:{x: args[1]*this.mapTileSize, y: args[2]*this.mapTileSize}})
        })
    }

    getRunner(node: string): Generator<BondageResults, BondageResults,undefined>{
        return this.runner.run(node)
    }
}
