import { __awaiter } from "tslib";
import 'phaser';
export default function (text, twindow, scene, speed) {
    return new Promise((done, reject) => {
        readChars(text, speed !== null && speed !== void 0 ? speed : 20, (char) => { twindow.appendMDText(char); }).then(v => done(undefined));
    });
}
function readChars(str, basedelay, cb) {
    return __awaiter(this, void 0, void 0, function* () {
        const length = str.length;
        for (let i = 0; i < length; ++i) {
            const char = str[i];
            cb(char);
            if (char == ' ') {
                yield timeout(basedelay * 1.7);
            }
            else {
                yield timeout(basedelay);
            }
        }
    });
}
function timeout(ms) {
    return new Promise(resolve => setTimeout(resolve, ms));
}
//# sourceMappingURL=typewriteText.js.map