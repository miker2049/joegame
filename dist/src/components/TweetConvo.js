import { __awaiter } from "tslib";
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
        return __awaiter(this, void 0, void 0, function* () {
            for (let conv of this.convo.reverse()) {
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
            // setTimeout(() => this.runConvo(), 5000)
        });
    }
}
//# sourceMappingURL=TweetConvo.js.map