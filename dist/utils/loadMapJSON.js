"use strict";

var _interopRequireDefault = require("@babel/runtime/helpers/interopRequireDefault");

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.default = _default;

require("phaser");

var _getKeyNames = require("./getKeyNames");

var _loadAfterLoad = _interopRequireDefault(require("./loadAfterLoad"));

function _default(game, path) {
  return new Promise(function (res, reject) {
    var scene = game.scene.getScenes(true, false)[0];
    (0, _loadAfterLoad.default)(scene, (0, _getKeyNames.getMapKeyNameRaw)(path), path).then(function (key) {
      return res(game);
    }).catch(function (err) {
      return console.log(err);
    });
  });
}