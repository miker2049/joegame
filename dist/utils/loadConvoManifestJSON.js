"use strict";

var _interopRequireDefault = require("@babel/runtime/helpers/interopRequireDefault");

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.default = _default;

require("phaser");

var _loadAfterLoad = _interopRequireDefault(require("./loadAfterLoad"));

function _default(game) {
  return new Promise(function (res, reject) {
    if (!game.cache.json.exists('gdata')) reject("No global data loaded");
    var scene = game.scene.getScenes(true, false)[0];
    (0, _loadAfterLoad.default)(scene, 'convo-manifest', scene.cache.json.get('gdata').convoManifest).then(function (key) {
      res(game);
    }).catch(function (err) {
      reject("Something wrong in retrieving convo manifest");
    });
  });
}
//# sourceMappingURL=loadConvoManifestJSON.js.map