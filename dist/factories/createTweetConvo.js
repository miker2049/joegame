import { __awaiter } from "tslib";
import loadAfterLoad from '../utils/loadAfterLoad';
import createCharacter from './createCharacter';
import wikiData from '../utils/wikiData';
import { Dir } from '../joegameTypes';
import TweetConvo from '../components/TweetConvo';
import shuffle from '../utils/shuffleArr';
/*
 * Until further notice this takes tile coords
 */
export default function (level, tx, ty, charGroup, convoID) {
    return __awaiter(this, void 0, void 0, function* () {
        const mani = level.scene.cache.json.get('convo-manifest');
        const convoIDD = mani[randomIndexx(mani)].match(/(\d+)(_single)?\.json$/)[1];
        const convoJsonPath = mani.find(entry => entry.match(convoIDD));
        // console.log('loading ' + convoIDD)
        yield loadAfterLoad(level.scene, convoIDD, 'assets/convos/' + convoJsonPath);
        // console.log('loaded ' + convoIDD)
        const convo = level.scene.cache.json.get(convoIDD);
        let users = Array.from(new Set(convo.map(tweet => tweet.username)));
        const charAmount = Math.min(users.length, 4);
        let listOfChars = Array.from(wikiData(level.scene.game).character);
        if (charGroup && charGroup !== 'all') {
            listOfChars = listOfChars.filter(char => {
                // console.log(char)
                if (char[1].charGroups) {
                    return char[1].charGroups.includes(charGroup);
                }
            });
        }
        listOfChars = shuffle(listOfChars).slice(0, charAmount);
        let chars = [];
        for (let i = 0; i < listOfChars.length; i++) {
            if (i === 0) {
                // to the west
                const char = createCharacter(listOfChars[i][0], tx - level.map.tileWidth, ty, level);
                char.face(Dir.east);
                chars.push(char);
            }
            else if (i === 1) {
                // to the north
                const char = createCharacter(listOfChars[i][0], tx, ty - level.map.tileHeight, level);
                char.face(Dir.south);
                chars.push(char);
            }
            else if (i === 2) {
                // to the east
                const char = createCharacter(listOfChars[i][0], tx + level.map.tileWidth, ty, level);
                char.face(Dir.west);
                chars.push(char);
            }
            else if (i === 3) {
                // to the south
                const char = createCharacter(listOfChars[i][0], tx, ty + level.map.tileHeight, level);
                char.face(Dir.north);
                chars.push(char);
            }
        }
        chars.forEach(c => level.scene.add.existing(c));
        // const characters = charAmount.map(n=>{
        //     if(n===1){
        //         createCharacter
        //     }
        // })
        // console.log(convo)
        let tconvo = new TweetConvo(chars, convo, users, level);
        return tconvo;
    });
}
function randomIndexx(arr) {
    return Math.floor(Math.random() * arr.length);
}
//# sourceMappingURL=createTweetConvo.js.map