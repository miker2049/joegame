import loadAfterLoad from '../utils/loadAfterLoad'
import { ILevelComponents } from '../ILevel'
import createCharacter from './createCharacter'
import wikiData from '../utils/wikiData'
import { wikiCharacterEntry } from '../utils/parseWikiData'
import { ICharacter } from '../ICharacter'
import { Dir } from '../joegameTypes'
import TweetConvo from '../components/TweetConvo'
import shuffle from '../utils/shuffleArr'
/*
 * Until further notice this takes tile coords
 */
export default async function(level: ILevelComponents, tx: number, ty: number, charGroup?: string, convoID?: string) {
    const mani: string[] = level.scene.cache.json.get('convo-manifest')
    const convoIDD = mani[randomIndexx(mani)].match(/(\d+)(_single)?\.json$/)![1]
    console.log(convoIDD)
    const convoJsonPath: string = mani.find(entry => entry.match(convoIDD))!
    await loadAfterLoad(level.scene, convoIDD, 'assets/convos/' + convoJsonPath, 'json')
    const convo = level.scene.cache.json.get(convoIDD) as any[]
    let users = Array.from(new Set(convo.map(tweet => tweet.username as string)))
    const charAmount = Math.min(users.length, 4)
    let listOfChars: [string, wikiCharacterEntry][] = Array.from(wikiData(level.scene.game).character)
    if (charGroup && charGroup !== 'all') {
        listOfChars = listOfChars.filter(char => {
            if (char[1].charGroups) {
                return char[1].charGroups.includes(charGroup)
            }
        })
    }

    listOfChars = shuffle(listOfChars).slice(0, charAmount)
    let chars: ICharacter[] = []
    for (let i = 0; i < listOfChars.length; i++) {
        if (i === 0) {
            // to the west
            const char = createCharacter(listOfChars[i][0], tx - level.map.tileWidth, ty, level)
            char.face(Dir.east)
            chars.push(char)
        } else if (i === 1) {
            // to the north
            const char = createCharacter(listOfChars[i][0], tx, ty - level.map.tileHeight, level)
            char.face(Dir.south)
            chars.push(char)
        } else if (i === 2) {
            // to the east
            const char = createCharacter(listOfChars[i][0], tx + level.map.tileWidth, ty, level)
            char.face(Dir.west)
            chars.push(char)
        } else if (i === 3) {
            // to the south
            const char = createCharacter(listOfChars[i][0], tx, ty + level.map.tileHeight, level)
            char.face(Dir.north)
            chars.push(char)
        }
    }

    chars.forEach(c => level.scene.add.existing(c))

    let tconvo = new TweetConvo(chars, convo, users as string[], level)
    console.log('end of tweet convo')
    return tconvo
}





function randomIndexx(arr: any[]) {
    return Math.floor(Math.random() * arr.length)
}
