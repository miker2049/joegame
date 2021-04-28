import { __awaiter } from "tslib";
import createTweetConvo from '../factories/createTweetConvo';
import shuffle from '../utils/shuffleArr';
export default function (level, layer) {
    var _a, _b, _c, _d, _e;
    return __awaiter(this, void 0, void 0, function* () {
        if (!level.map.getObjectLayer(layer)) {
            return;
        }
        let convos = [];
        let mani = JSON.parse(JSON.stringify(level.scene.cache.json.get('convo-manifest')));
        for (let obj_ of level.map.getObjectLayer(layer).objects) {
            let convoIDD;
            // const coord = level.map.tileToWorldXY(obj_.x, obj_.y)
            const charGroup = (_c = (_b = (_a = obj_.properties) === null || _a === void 0 ? void 0 : _a.find((prop) => prop.name === 'charGroup')) === null || _b === void 0 ? void 0 : _b.value) !== null && _c !== void 0 ? _c : 'all';
            if (mani.length > 0) {
                convoIDD = shuffle(mani).pop();
            }
            else {
                mani = JSON.parse(JSON.stringify(level.scene.cache.json.get('convo-manifest')));
                mani = mani.files;
                convoIDD = shuffle(mani).pop();
            }
            convos.push(yield createTweetConvo(level, (_d = obj_.x) !== null && _d !== void 0 ? _d : 0, (_e = obj_.y) !== null && _e !== void 0 ? _e : 0, charGroup, convoIDD));
        }
        return convos;
    });
}
//# sourceMappingURL=addAllTweetConvosFromLayer.js.map