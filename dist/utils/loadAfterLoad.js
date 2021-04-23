import { __awaiter } from "tslib";
import 'phaser';
export default function (scene, key, url) {
    return __awaiter(this, void 0, void 0, function* () {
        return new Promise((res, rej) => {
            scene.load.json(key, url);
            if (scene.cache.json.exists(key)) {
                res(key);
            }
            scene.load.once('filecomplete', (keyy) => {
                // console.log(keyy)
                if (keyy === key) {
                    res(key);
                }
            });
            scene.load.once('loaderror', (file) => {
                if (file.key === key) {
                    rej(file);
                }
            });
            scene.load.start();
        });
    });
}
//# sourceMappingURL=loadAfterLoad.js.map