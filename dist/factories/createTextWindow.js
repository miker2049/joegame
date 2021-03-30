"use strict";

var _interopRequireDefault = require("@babel/runtime/helpers/interopRequireDefault");

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.default = _default;

require("phaser");

var _TextWindow = _interopRequireDefault(require("../components/TextWindow"));

function _default(config) {
  var _config$x, _config$y, _config$width, _config$height, _config$text, _config$additionalSty;

  return config.game.scene.add("textwindow", new _TextWindow.default({
    key: "textwindow_scene",
    physics: {}
  }), true, {
    x: (_config$x = config.x) !== null && _config$x !== void 0 ? _config$x : config.game.renderer.width / 2,
    y: (_config$y = config.y) !== null && _config$y !== void 0 ? _config$y : config.game.renderer.height / 2,
    width: (_config$width = config.width) !== null && _config$width !== void 0 ? _config$width : 300,
    height: (_config$height = config.height) !== null && _config$height !== void 0 ? _config$height : 300,
    text: (_config$text = config.text) !== null && _config$text !== void 0 ? _config$text : '',
    additionalStyle: (_config$additionalSty = config.additionalStyle) !== null && _config$additionalSty !== void 0 ? _config$additionalSty : ''
  });
}