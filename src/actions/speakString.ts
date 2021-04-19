import 'phaser'
import { chunk } from 'lodash'
import { ILevelComponents } from '../ILevel'
import timeout from '../utils/awaitTimeout'
import globalDefaults from '../defaults'
import syllableCount from '../utils/syllableCount'
import { ITalkingPlayConfig } from '../sound/synths/Talking'
import hashToArr from '../utils/hashToArr'
import { assert } from 'tone/build/esm/core/util/Debug'
export default async function(str: string, char: { x: number, y: number, scene: Phaser.Scene }, speakFunc: (config: ITalkingPlayConfig) => void, speed?: number): Promise<void> {
    // if (!(Phaser.Geom.Rectangle.ContainsPoint(char.scene.cameras.main.getBounds(), new Phaser.Geom.Point(char.x, char.y)))) return

    const words = str.split(' ')
    const wlength = words.length
    // const ttime = ((speed ?? globalDefaults.talkingSpeed) * str.replace(' ', '').length) +
    //     ((str.match(/\s/g)?.length ?? 0) * (globalDefaults.talkingSpeed * 1.7))
    for (let i = 0; i < wlength; i++) {
        // console.log(hashCode(words[i]))
        const syllable = syllableCount(words[i])
        const randArr = randArrayAndSum(syllable)
        // need two indexes for each syllable, for vowel (buff) and rate (pitch)
        // TODO remove lodash dependency!!!
        const numbers = chunk(hashToArr(words[i], syllable * 2), 2)

        assert(randArr[0].length === numbers.length, 'not same laengthss')
        for (let j = 0; j < randArr[0].length; j++) {
            const delay = (randArr[0][j] / randArr[1]) * (words[i].length * globalDefaults.talkingSpeed)
            if (char.scene.cameras.main.worldView.contains(char.x, char.y) == true) {
                const vAndp = getVolAndPanFromDistance(
                    char.scene.cameras.main.worldView.centerX,
                    char.scene.cameras.main.worldView.centerX,
                    char.x,
                    char.y,
                    char.scene.cameras.main.worldView.width
                )
                speakFunc({
                    inst: 'talking',
                    buff: numbers[j][0] ?? undefined,
                    rate: numbers[j][1] ?? undefined,
                    vol: vAndp[0],
                    pan: vAndp[1]
                })
            }
            await timeout(delay)
        }
        await timeout(globalDefaults.talkingSpeed * 1.7)
    }
    return
}

function randArrayAndSum(length: number): [number[], number] {
    let sum = 0
    let arr = []
    for (let i = 0; i < length; i++) {
        arr.push(Math.random())
    }
    return [arr, arr.reduce((pr, val) => pr + val)]
}

function clump(arr: any[], n: number) {
    if (arr.length < n) return [arr]
    let i = 0, out = []

}


type volAndPan = [vol: number, pan: number]
function getVolAndPanFromDistance(playerX: number, playerY: number, charX: number, charY: number, cameraWidth: number): volAndPan {
    const difference = charX - playerX
    const distance = Math.sqrt(difference ^ 2 + (charY - playerY) ^ 2)
    const mod = difference > 0 ? -1 : 1
    const pan = difference / (cameraWidth / 2)
    const vol = distance / (cameraWidth / 2)
    return [vol, pan]
}
