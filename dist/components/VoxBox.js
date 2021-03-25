import { __awaiter } from "tslib";
import 'phaser';
import typewriteText from '../utils/typewriteText';
const BOXALPHA = 0.7;
export default class VoxBox extends Phaser.GameObjects.Text {
    constructor(level) {
        super(level.scene, 0, 4, '', {
            // fontFamily: 'Retro Gaming',
            fontSize: '12px',
            wordWrap: {
                width: level.map.tileWidth * 7,
            },
            padding: { x: 2, y: 2 },
            fixedWidth: level.map.tileWidth * 7,
            fixedHeight: level.map.tileWidth * 3.5,
        });
        this.setWordWrapCallback((str) => {
            const wrapped = this.basicWordWrap(str, this.context, level.map.tileWidth * 7);
            let splitt = wrapped.split('\n');
            return splitt.slice(-5);
        });
        this.textbuff = '';
        this.setAlpha(BOXALPHA);
        this.setBackgroundColor('black');
        this.setOrigin(0.5, 1);
        this.setScale(1 / (level.scene.cameras.default.zoom * 2));
        // this.setMaxLines
    }
    speak(str, speed) {
        return __awaiter(this, void 0, void 0, function* () {
            this.setMDText('');
            this.open();
            yield typewriteText(str, this, this.scene, speed);
            // this.setText(str)
            // this.scene.time.addEvent({
            //     delay: 3500,
            //     callback: () => {
            //         this.close()
            //     }
            // })
        });
    }
    open() {
        this.scene.tweens.add({
            targets: [this],
            alpha: BOXALPHA,
            duration: 500
        });
    }
    close() {
        this.scene.tweens.add({
            targets: [this],
            alpha: 0,
            duration: 500
        });
    }
    //TODO stop lying about this
    setMDText(text) {
        this.textbuff = text;
        this.updateVoxtext();
    }
    appendMDText(text) {
        this.textbuff += text;
        this.updateVoxtext();
    }
    appendNewLineMDText(text) {
        this.textbuff += "\n\n" + text;
        this.updateVoxtext();
    }
    updateVoxtext() {
        this.setText(this.textbuff);
        this.updateText();
    }
}
//# sourceMappingURL=VoxBox.js.map