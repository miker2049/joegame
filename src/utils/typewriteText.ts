import { ITextBox } from '../components/TextWindow'
import timeout from './awaitTimeout'
import global from '../defaults'
import 'phaser'

export function typewriteText(text: string, twindow: ITextBox, scene: Phaser.Scene, speed?: number): Promise<undefined> {
    return new Promise((done, reject) => {
        readChars(text, speed ?? global.talkingSpeed, (char: string) => {
            twindow.appendMDText(char)
        }).then(v => done(undefined))
    })
}

async function readChars(str: string, basedelay: number, cb: (T: string) => void) {
    const length = str.length
    let debugTime = 0
    for (let i = 0; i < length; ++i) {
        const char = str[i]
        cb(char)
        if (char == ' ') {
            debugTime += basedelay * 1.7
            await timeout(basedelay * 1.7)
        } else {
            debugTime += basedelay
            await timeout(basedelay)
        }
    }
    console.log(`debugTime readChars ${debugTime}`)

}


