import { __asyncValues, __awaiter } from "tslib";
import { createNPCMachine } from './NPCMachine';
import randomInterestSet from '../utils/randomInterestSet';
import { interpret } from 'xstate';
import defaults from '../defaults';
const urlRegex = /((([A-Za-z]{3,9}:(?:\/\/)?)(?:[\-;:&=\+\$,\w]+@)?[A-Za-z0-9\.\-]+|(?:www\.|[\-;:&=\+\$,\w]+@)[A-Za-z0-9\.\-]+)((?:\/[\+~%\/\.\w\-_]*)?\??(?:[\-\+=&;%@\.\w_]*)#?(?:[\.\!\/\\\w]*))?)/g;
export default class TweetConvo {
    // box:
    constructor(chars, convo, users, level) {
        this.chars = chars;
        this.convo = convo;
        this.users = users;
        // this.chars[0].
        this.chars.forEach((char) => {
            if (true) {
                // if ((() => Math.random())() > 0.5) {
                let mach = createNPCMachine(char, level.map.tileWidth, level.pathfinder, randomInterestSet(level.map, 4, { x: char.x, y: char.y }));
                let intt = interpret(mach);
                // intt.start()
            }
        });
        // this.chars.forEach(char=>{
        //     char.scene.physics.add.overlap(char,level.player,)
        // })
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
                    let text = conv.text.replace(/@[^\s]*/g, '');
                    text = text.replace(urlRegex, '');
                    text = text.replace(/\s\s+/g, '');
                    this.chars[charIndex].jumpUp();
                    this.chars.forEach(char => char.setDepth(defaults.charDepth));
                    this.chars[charIndex].setDepth(10);
                    yield this.chars[charIndex].speak(text, 45);
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
            setTimeout(() => this.runConvo(), 5000);
        });
    }
}
//# sourceMappingURL=TweetConvo.js.map