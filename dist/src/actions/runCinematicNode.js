import { __awaiter } from "tslib";
import 'phaser';
import createCineRunner from '../factories/createCineRunner';
import createTextWindow from '../factories/createTextWindow';
import { getDialogueKeyName } from '../utils/getKeyNames';
import { typewriteText } from '../utils/typewriteText';
export default function (level, node) {
    return __awaiter(this, void 0, void 0, function* () {
        const yarnjson = level.scene.cache.json.get(getDialogueKeyName(level.key));
        const textWindow = createTextWindow({
            game: level.scene.game,
            x: 20,
            y: 20,
            width: level.scene.renderer.width * (2 / 3),
            height: level.scene.renderer.height * (1 / 3),
            additionalStyle: "padding-top: 1em"
        });
        const runner = createCineRunner(level, yarnjson, textWindow);
        for (let result of runner.run(node)) {
            switch (result.constructor.name) {
                case "TextResult": {
                    yield typewriteText(result.text, textWindow, level.scene, 50);
                    textWindow.appendNewLineMDText('');
                    yield new Promise(resolve => setTimeout(resolve, 1000));
                    break;
                }
                case "CommandResult": {
                    const command = result;
                    console.log(command);
                    if (command.result instanceof Promise) {
                        yield command.result;
                    }
                    break;
                }
                case "OptionsResult": {
                    break;
                }
            }
        }
    });
}
//# sourceMappingURL=runCinematicNode.js.map