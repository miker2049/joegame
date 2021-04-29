import 'phaser'
import { Runner, YarnNode } from 'bondage'
import { ITextWindow } from '../components/TextWindow'
import { ILevelComponents } from '../ILevel'

export default function(level: ILevelComponents,
    yarnjson: YarnNode[],
    textWindow: ITextWindow,
): Runner {
    const runner = new Runner()
    runner.load(yarnjson)
    runner.setVariableStorage(level.scene.registry)

    //NOTE assuming square tile
    const tileSize = level.map.tileWidth

    runner.registerFunction('wait', async (args: [number]) => {
        await new Promise(resolve => { setTimeout(resolve, args[0]) })
    })
    runner.registerFunction('moveChar', (args: [string, number, number, string]) => {
        level.machineRegistry.sendTo(args[0], { type: 'MOVE_ON_PATH', point: { x: args[1] * tileSize, y: args[2] * tileSize } })
    })
    runner.registerFunction('moveCharSync', (args: [string, number, number, string]) => {
        level.machineRegistry.sendTo(args[0], { type: 'MOVE_ON_PATH', point: { x: args[1] * tileSize, y: args[2] * tileSize } })
        return new Promise(resolve => {
            level.machineRegistry.machines.get(args[0])!.onTransition(state => {
                if (state.value === 'still') {
                    resolve(null)
                }
            })
        })
    })
    runner.registerFunction('transportChar', (args: [string, number, number, string]) => {
        level.machineRegistry.sendTo(args[0], { type: 'TRANSPORT', point: { x: args[1] * tileSize, y: args[2] * tileSize } })
    })
    runner.registerFunction('openWindow', (_args) => {
        textWindow.open()
    })
    runner.registerFunction('closeWindow', (_args) => {
        textWindow.close()
    })
    runner.registerFunction('clearWindowText', (_args) => {
        textWindow.setMDText('')
    })





    return runner
}
