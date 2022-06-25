import 'phaser'
import bondage, {YarnNode} from 'dialogue'
import { ITextBox } from '../components/TextWindow'
import { ILevelComponents } from '../ILevel'
const Runner = bondage.Runner
export default function(level: ILevelComponents,
    yarndata: YarnNode[] | string,
    textWindow: ITextBox,
): bondage.Runner {
    const runner = new Runner()
    if (typeof yarndata == "string") {

        runner.loadYarnString(yarndata)
    } else {

        runner.load(yarndata)
    }

    // console.log(yarndata)
    // console.log(runner)
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
            level.machineRegistry.machines.get(args[0])!.onTransition((state: any) => {
                if (state.value === 'still') {
                    resolve(null)
                }
            })
        })
    })
    runner.registerFunction('transportChar', (args: [string, number, number, string]) => {
        level.machineRegistry.sendTo(args[0], { type: 'TRANSPORT', point: { x: args[1] * tileSize, y: args[2] * tileSize } })
    })
    runner.registerFunction('openWindow', (_args: any[]) => {
        textWindow.open()
    })
    runner.registerFunction('closeWindow', (_args: any) => {
        textWindow.close()
    })

    // x, y
    runner.registerFunction('moveWindowDownRight', _ => {
        textWindow.x = level.scene.renderer.width - textWindow.width - 20
        textWindow.y = level.scene.renderer.height - textWindow.height - 20
    })

    runner.registerFunction('clearWindowText', (_args: any) => {
        textWindow.setMDText('')
    })
    // x, y, speed
    runner.registerFunction('moveCamera', (args: [number, number, number]) => {
        level.scene.cameras.main.stopFollow()
        const cam = level.scene.cameras.main
        const xDiff = ((args[0] * tileSize + tileSize / 2) - cam.worldView.centerX)
        const yDiff = ((args[1] * tileSize + tileSize / 2) - cam.worldView.centerY)
        level.scene.tweens.add({
            targets: cam,
            scrollX: {
                from: cam.scrollX,
                to: cam.scrollX + xDiff
            },
            scrollY: {
                from: cam.scrollY,
                to: cam.scrollY + yDiff
            },
            duration: args[2],
            ease: 'Linear'
        })
    })
    // zoom, dur
    runner.registerFunction('zoomCamera', (args: [number, number]) => {
        level.scene.cameras.main.stopFollow()
        const cam = level.scene.cameras.main
        level.scene.tweens.add({
            targets: cam,
            zoom: args[0],
            duration: args[1],
            ease: 'Linear'
        })
    })

    // dur arg TODO
    runner.registerFunction('resetCameraPlayer', (_args: [number]) => {
        if (level.player) {
            level.scene.cameras.main.startFollow(level.player)

        }
    })

    return runner
}
