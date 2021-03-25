import { __asyncValues, __awaiter } from "tslib";
import createTweetConvo from '../factories/createTweetConvo';
import shuffle from '../utils/shuffleArr';
export default function (level, layer) {
    var e_1, _a;
    var _b, _c, _d;
    return __awaiter(this, void 0, void 0, function* () {
        if (!level.map.getObjectLayer(layer)) {
            return;
        }
        let convos = [];
        let mani = JSON.parse(JSON.stringify(level.scene.cache.json.get('convo-manifest')));
        try {
            for (var _e = __asyncValues(level.map.getObjectLayer(layer).objects), _f; _f = yield _e.next(), !_f.done;) {
                let obj_ = _f.value;
                let convoIDD;
                // const coord = level.map.tileToWorldXY(obj_.x, obj_.y)
                const charGroup = (_d = (_c = (_b = obj_.properties) === null || _b === void 0 ? void 0 : _b.find(prop => prop.name === 'charGroup')) === null || _c === void 0 ? void 0 : _c.value) !== null && _d !== void 0 ? _d : 'all';
                if (mani.length > 0) {
                    convoIDD = shuffle(mani).pop();
                }
                else {
                    mani = JSON.parse(JSON.stringify(level.scene.cache.json.get('convo-manifest')));
                    mani = mani.files;
                    convoIDD = shuffle(mani).pop();
                }
                convos.push(yield createTweetConvo(level, obj_.x, obj_.y, charGroup, convoIDD));
            }
        }
        catch (e_1_1) { e_1 = { error: e_1_1 }; }
        finally {
            try {
                if (_f && !_f.done && (_a = _e.return)) yield _a.call(_e);
            }
            finally { if (e_1) throw e_1.error; }
        }
        return convos;
    });
}
//# sourceMappingURL=addAllTweetConvosFromLayer.js.map