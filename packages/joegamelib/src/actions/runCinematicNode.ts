import { CommandResult, TextResult } from 'dialogue'
import { ICharacter } from 'ICharacter'
import createCineRunner from '../factories/createCineRunner'
import createTextWindow from '../factories/createTextWindow'
import { ILevelComponents } from '../ILevel'
import { typewriteText } from '../utils/typewriteText'

export default async function(level: ILevelComponents, node: string, dialoguedata:string|any) {
    const textWindow = createTextWindow({
        game: level.scene.game,
        x: 20,
        y: 20,
        width: level.scene.renderer.width * (2 / 3),
        height: level.scene.renderer.height * (1 / 3),
        additionalStyle: "padding-top: 1em"
    })
    const runner = createCineRunner(level, dialoguedata, textWindow)
    for (let result of runner.run(node)) {
        switch (result.constructor.name) {
            case "TextResult": {
                const speakerParse = (result as TextResult).text.match(/(^\w+\:)(.*)/)
                if(speakerParse){
                    const npc = level.npcs.getMatching('name', speakerParse[1].slice(0,-1))
                    if(npc){
                      await (npc[0] as ICharacter).speak(speakerParse[2])
                    }
                } else {
                    await typewriteText((result as TextResult).text, textWindow, level.scene, 50)
                    textWindow.appendNewLineMDText('')
                }
                await new Promise(resolve => setTimeout(resolve, (()=>Math.random()*700+500)()))
                break;
            }
            case "CommandResult": {
                const command = result as CommandResult
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
