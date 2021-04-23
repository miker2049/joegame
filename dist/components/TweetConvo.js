import { __asyncValues, __awaiter } from "tslib";
import defaults from '../defaults';
const urlRegex = /((([A-Za-z]{3,9}:(?:\/\/)?)(?:[\-;:&=\+\$,\w]+@)?[A-Za-z0-9\.\-]+|(?:www\.|[\-;:&=\+\$,\w]+@)[A-Za-z0-9\.\-]+)((?:\/[\+~%\/\.\w\-_]*)?\??(?:[\-\+=&;%@\.\w_]*)#?(?:[\.\!\/\\\w]*))?)/g;
export default class TweetConvo {
    // box:
    constructor(chars, convo, users, level) {
        this.chars = chars;
        this.convo = convo;
        this.users = users;
    }
    runConvo() {
        var e_1, _a;
        return __awaiter(this, void 0, void 0, function* () {
            try {
                for (var _b = __asyncValues(this.convo.reverse()), _c; _c = yield _b.next(), !_c.done;) {
                    let conv = _c.value;
                    const charIndex = this.users.findIndex((val) => {
                        return val === conv.username;
                    }) % this.chars.length;
                    let text = conv.text.replace(/@[^\s]*/g, '')
                        .replace(urlRegex, '')
                        .replace(/\s\s+/g, '')
                        .replace(/[\.\?\!\;\:]/g, '$&  ');
                    this.chars[charIndex].jumpUp();
                    this.chars.forEach(char => char.setDepth(defaults.charDepth));
                    this.chars[charIndex].setDepth(10);
                    yield this.chars[charIndex].speak(text, defaults.talkingSpeed);
                    this.chars[charIndex].voxbox.close();
                }
            }
            catch (e_1_1) { e_1 = { error: e_1_1 }; }
            finally {
                try {
                    if (_c && !_c.done && (_a = _b.return)) yield _a.call(_b);
                }
                finally { if (e_1) throw e_1.error; }
            }
        });
    }
}
//# sourceMappingURL=TweetConvo.js.map