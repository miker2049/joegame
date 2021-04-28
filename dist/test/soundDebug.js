import { __awaiter } from "tslib";
import { Toner } from '../src/index';
const acontext = new AudioContext();
function createPlayerUI(name, toner) {
    const div = document.createElement('div');
    const playbuttonid = `playbutton-${name}`;
    div.innerHTML = `
<span><h2>${name}</h2><button id=${playbuttonid}>Play</button></span>
`;
    document.body.appendChild(div);
    document.querySelector("#" + playbuttonid).addEventListener('click', function () {
        return __awaiter(this, void 0, void 0, function* () {
            yield acontext.resume();
            toner.play(name);
        });
    });
}
const toner = new Toner(acontext);
createPlayerUI('arp', toner);
createPlayerUI('walk', toner);
createPlayerUI('gong', toner);
createPlayerUI('talking', toner);
//# sourceMappingURL=soundDebug.js.map