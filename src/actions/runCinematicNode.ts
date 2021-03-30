import { CommandResult, TextResult } from 'bondage'
import 'phaser'
import createCineRunner from '../factories/createCineRunner'
import createTextWindow from '../factories/createTextWindow'
import { ILevelComponents } from '../ILevel'
import { getDialogueKeyName } from '../utils/getKeyNames'
import typewriteText from '../utils/typewriteText'

export default async function(level: ILevelComponents, node: string) {
    const yarnjson = level.scene.cache.json.get(getDialogueKeyName(level.key))
    const textWindow = createTextWindow({
        game: level.scene.game,
        x: 20,
        y: 20,
        width: level.scene.renderer.width * (2 / 3),
        height: level.scene.renderer.height * (1 / 3),
        additionalStyle: "padding-top: 1em"
    })
    const runner = createCineRunner(level, yarnjson, textWindow)
    for (let result of runner.run(node)) {
        switch (result.constructor.name) {
            case "TextResult": {
                await typewriteText((result as TextResult).text, textWindow, level.scene, 50)
                textWindow.appendNewLineMDText('')
                await new Promise(resolve => setTimeout(resolve, 1000))
                break;
            }
            case "CommandResult": {
                const command = result as CommandResult
                console.log(command)
                if (command.result instanceof Promise) {
                    await command.result
                }
                break;
            }
            case "OptionsResult": {
                break;
            }
        }
    }
}
