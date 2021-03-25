import { ITextBox } from '../components/TextWindow'
import 'phaser'

export default function(text: string, twindow: ITextBox, scene: Phaser.Scene, speed?: number): Promise<undefined> {
    return new Promise((done, reject) => {
        readChars(text, speed ?? 20, (char: string) => { twindow.appendMDText(char) }).then(v => done(undefined))
    })
}

async function readChars(str: string, basedelay: number, cb: (T: string) => void) {
    const length = str.length
    for (let i = 0; i < length; ++i) {
        const char = str[i]
        cb(char)
        if (char == ' ') {
            await timeout(basedelay * 1.7)
        } else {
            await timeout(basedelay)
        }
    }

}

function timeout(ms) {
    return new Promise(resolve => setTimeout(resolve, ms))
}
