import { __awaiter } from "tslib";
import 'phaser';
import timeout from '../utils/awaitTimeout';
import globalDefaults from '../defaults';
import { syllableCount } from '../utils/syllableCount';
import { hashToArr } from '../utils/hashToArr';
import { assert } from 'tone/build/esm/core/util/Debug';
import { getVolAndPanFromDistance } from '../utils/getVolPanFromDist';
export default function (str, char, speakFunc, speed) {
    var _a, _b;
    return __awaiter(this, void 0, void 0, function* () {
        // if (!(Phaser.Geom.Rectangle.ContainsPoint(char.scene.cameras.main.getBounds(), new Phaser.Geom.Point(char.x, char.y)))) return
        const words = str.replace(/[^A-Za-z0-9]/g, ' ').split(' ');
        const wlength = words.length;
        // const ttime = ((speed ?? globalDefaults.talkingSpeed) * str.replace(' ', '').length) +
        //     ((str.match(/\s/g)?.length ?? 0) * (globalDefaults.talkingSpeed * 1.7))
        for (let i = 0; i < wlength; i++) {
            // console.log(hashCode(words[i]))
            const syllable = syllableCount(words[i]);
            if (words[i].length > 0) {
                const randArr = randArrayAndSum(syllable);
                // need two indexes for each syllable, for vowel (buff) and rate (pitch)
                // TODO remove lodash dependency!!!
                const numbers = chunk2(hashToArr(words[i], syllable * 2), 2);
                assert(hashToArr(words[i], syllable * 2).length == syllable * 2, `hashtoArr returns the right word:"${words[i]}"  ${syllable} AND ${hashToArr(words[i], syllable * 2)}`);
                assert(randArr[0].length === numbers.length, `not same laengthss ${words[i]} ${randArr[0].length}  ${numbers.length}`);
                for (let j = 0; j < randArr[0].length; j++) {
                    const delay = (randArr[0][j] / randArr[1]) * (words[i].length * globalDefaults.talkingSpeed);
                    const loc = {
                        x: char.x || char.scene.cameras.main.worldView.centerX,
                        y: char.y || char.scene.cameras.main.worldView.centerY,
                    };
                    if (char.scene.cameras.main.worldView.contains(loc.x, loc.y) == true) {
                        const vAndp = getVolAndPanFromDistance(char.scene.cameras.main.worldView.centerX, char.scene.cameras.main.worldView.centerX, loc.x, loc.y, char.scene.cameras.main.worldView.width);
                        speakFunc({
                            inst: 'talking',
                            buff: (_a = numbers[j][0]) !== null && _a !== void 0 ? _a : undefined,
                            rate: (_b = numbers[j][1]) !== null && _b !== void 0 ? _b : undefined,
                            vol: vAndp[0],
                            pan: vAndp[1]
                        });
                    }
                    yield timeout(delay);
                }
                yield timeout(globalDefaults.talkingSpeed * 2);
            }
        }
        return;
    });
}
function randArrayAndSum(length) {
    let sum = 0;
    let arr = [];
    for (let i = 0; i < length; i++) {
        arr.push(Math.random());
    }
    return [arr, arr.reduce((pr, val) => pr + val)];
}
function chunk2(arr, n) {
    let out = [];
    for (let i = 0; i < arr.length; i += n) {
        out.push(arr.slice(i, i + n));
    }
    return out;
}
//# sourceMappingURL=speakString.js.map