import 'phaser';
import mdParse from '../utils/mdParse';
export default class TextWindow extends Phaser.Scene {
    constructor() {
        super(...arguments);
        this.textBuff = '';
    }
    init(data) {
        var _a, _b, _c;
        const style = ` list-style: none;
             background-color: rgba(0,0,0,0.8);
             overflow: hidden;
             width:${(_a = data.width) !== null && _a !== void 0 ? _a : 250}px;
             margin: 0px;
             padding: 1em;
             color: white;
             font-family: Retro Gaming;
             height:${(_b = data.height) !== null && _b !== void 0 ? _b : 250}px;
             -ms-overflow-style:none;
             scrollbar-width: none;`;
        this.phaserDom = this.add.dom(data.x, data.y, 'div', (_c = style + data.additionalStyle) !== null && _c !== void 0 ? _c : '').setOrigin(0, 0);
        if (data.text) {
            this.setMDText(data.text);
        }
        this.input.keyboard.enabled = true;
        this.phaserDom.alpha = 0;
        this.input.keyboard.on('keyup', (event) => {
            if (event.key === 'Enter' || event.key === 'Escape') {
                this.close();
            }
        });
    }
    open() {
        this.tweens.add({
            targets: [this.phaserDom],
            alpha: 1,
            duration: 500
        });
    }
    close() {
        this.input.keyboard.enabled = false;
        this.tweens.add({
            targets: [this.phaserDom],
            alpha: 0,
            duration: 500
        });
    }
    setMDText(text) {
        this.textBuff = text;
        this.updateHTML();
    }
    appendMDText(text) {
        this.textBuff += text;
        this.updateHTML();
    }
    appendNewLineMDText(text) {
        this.textBuff += "\n\n" + text;
        this.updateHTML();
    }
    updateHTML() { this.phaserDom.setHTML(mdParse(this.textBuff)); }
}
//# sourceMappingURL=TextWindow.js.map