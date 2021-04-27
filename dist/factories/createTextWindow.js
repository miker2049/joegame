import 'phaser';
import { TextWindow } from '../components/TextWindow';
export default function (config) {
    var _a, _b, _c, _d, _e, _f;
    return config.game.scene.add("textwindow", new TextWindow({ key: "textwindow_scene", physics: {} }), true, {
        x: (_a = config.x) !== null && _a !== void 0 ? _a : config.game.renderer.width / 2,
        y: (_b = config.y) !== null && _b !== void 0 ? _b : config.game.renderer.height / 2,
        width: (_c = config.width) !== null && _c !== void 0 ? _c : 300,
        height: (_d = config.height) !== null && _d !== void 0 ? _d : 300,
        text: (_e = config.text) !== null && _e !== void 0 ? _e : '',
        additionalStyle: (_f = config.additionalStyle) !== null && _f !== void 0 ? _f : ''
    });
}
//# sourceMappingURL=createTextWindow.js.map