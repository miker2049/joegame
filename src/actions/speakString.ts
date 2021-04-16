import 'phaser'
import { ILevelComponents } from '../ILevel'
import timeout from '../utils/awaitTimeout'
import globalDefaults from '../defaults'
export default async function(str: string,
    char: Phaser.GameObjects.Image,
    speakFunc: (vol?: number, pan?: number) => void,
    player?: Phaser.GameObjects.Image
    speed?: number
): Promise<void> {
    if (!(Phaser.Geom.Rectangle.ContainsPoint(char.scene.cameras.main.getBounds(), new Phaser.Geom.Point(char.x, char.y)))) return

    const words = str.split(' ')
    const wlength = words.length
    const ttime = ((speed ?? globalDefaults.talkingSpeed) * str.replace(' ', '').length) +
        ((str.match(/\s/g)?.length ?? 0) * (globalDefaults.talkingSpeed * 1.7))
    for (let i = 0; i < wlength; i++) {
        const syllable = syllableCount(words[i])
        const randArr = randArrayAndSum(syllable)
        // for(let j = 0; j< syllable;j++){

        // }
        randArr[0].forEach(async (val) => {
            speakFunc()
            await timeout((val / randArr[1]) * (words[i].length * globalDefaults.talkingSpeed))
        })
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

// https://dl.acm.org/doi/10.1145/10563.10583
//  Each vowel (a, e, i, o, u, y) in a word counts as one syllable
//  subject to the following sub-rules:
// - Ignore final -ES, -ED, E (except for -LE)
// - Words of three letters or less count as one syllable
// - Consecutive vowels count as one syllable.
//
// https://stackoverflow.com/questions/5686483/how-to-compute-number-of-syllables-in-a-word-in-javascript
function syllableCount(word: string): number {
    word = word.toLowerCase();
    if (word.length <= 3) { return 1; }
    word = word.replace(/(?:[^laeiouy]es|ed|[^laeiouy]e)$/, '');
    word = word.replace(/^y/, '');
    return word.match(/[aeiouy]{1,2}/g)?.length ?? 1;
}
