import 'phaser'
// import { chunk } from 'lodash'
import { ILevelComponents } from '../ILevel'
import timeout from '../utils/awaitTimeout'
import globalDefaults from '../defaults'
import { syllableCount } from '../utils/syllableCount'
import { ITalkingPlayConfig } from '../sound/synths/Talking'
import { hashToArr } from '../utils/hashToArr'
import { assert } from 'tone/build/esm/core/util/Debug'
import { getVolAndPanFromDistance } from '../utils/getVolPanFromDist'
export default async function(str: string, char: { x?: number, y?: number, name?: string, scene: Phaser.Scene, }, speakFunc: (config: ITalkingPlayConfig) => void, speed?: number): Promise<void> {
    // if (!(Phaser.Geom.Rectangle.ContainsPoint(char.scene.cameras.main.getBounds(), new Phaser.Geom.Point(char.x, char.y)))) return

    const words = str.replace(/[^A-Za-z0-9]/g, ' ').split(' ')
    const wlength = words.length
    // const ttime = ((speed ?? globalDefaults.talkingSpeed) * str.replace(' ', '').length) +
    //     ((str.match(/\s/g)?.length ?? 0) * (globalDefaults.talkingSpeed * 1.7))
    for (let i = 0; i < wlength; i++) {
        // console.log(hashCode(words[i]))
        const syllable = syllableCount(words[i])
        if (words[i].length > 0) {
            const randArr = randArrayAndSum(syllable)
            // need two indexes for each syllable, for vowel (buff) and rate (pitch)
            // TODO remove lodash dependency!!!
            const numbers = chunk2(hashToArr(words[i], syllable * 2), 2)
            assert(hashToArr(words[i], syllable * 2).length == syllable * 2, `hashtoArr returns the right word:"${words[i]}"  ${syllable} AND ${hashToArr(words[i], syllable * 2)}`)

            assert(randArr[0].length === numbers.length, `not same laengthss ${words[i]} ${randArr[0].length}  ${numbers.length}`)
            for (let j = 0; j < randArr[0].length; j++) {
                const delay = (randArr[0][j] / randArr[1]) * (words[i].length * globalDefaults.talkingSpeed)
                const loc = {
                    x: char.x || char.scene.cameras.main.worldView.centerX,
                    y: char.y || char.scene.cameras.main.worldView.centerY,
                }
                if (char.scene.cameras.main.worldView.contains(loc.x, loc.y) == true) {
                    const vAndp = getVolAndPanFromDistance(
                        char.scene.cameras.main.worldView.centerX,
                        char.scene.cameras.main.worldView.centerX,
                        loc.x,
                        loc.y,
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
            await timeout(globalDefaults.talkingSpeed * 2)
        }
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

function chunk2(arr: any[], n: number): number[][] {
    let out = []
    for (let i = 0; i < arr.length; i += n) {
        out.push(arr.slice(i, i + n))
    }
    return out
}
