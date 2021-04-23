import { __awaiter } from "tslib";
import timeout from './awaitTimeout';
import global from '../defaults';
import 'phaser';
export function typewriteText(text, twindow, scene, speed) {
    return new Promise((done, reject) => {
        readChars(text, speed !== null && speed !== void 0 ? speed : global.talkingSpeed, (char) => {
            twindow.appendMDText(char);
        }).then(v => done(undefined));
    });
}
function readChars(str, basedelay, cb) {
    return __awaiter(this, void 0, void 0, function* () {
        const length = str.length;
        // let debugTime = 0
        for (let i = 0; i < length; ++i) {
            const char = str[i];
            cb(char);
            if (char == ' ') {
                // debugTime += basedelay * 1.7
                yield timeout(basedelay * 1.7);
            }
            else {
                // debugTime += basedelay
                yield timeout(basedelay);
            }
        }
        // console.log(`debugTime readChars ${debugTime}`)
    });
}
//# sourceMappingURL=typewriteText.js.map