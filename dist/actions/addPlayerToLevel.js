"use strict";

var _interopRequireDefault = require("@babel/runtime/helpers/interopRequireDefault");

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.default = _default;

require("phaser");

var _createPlayer = _interopRequireDefault(require("../factories/createPlayer"));

function _default(level, x, y, char) {
  var player = (0, _createPlayer.default)(char ? char : "player", x, y, level);
  level.scene.add.existing(player);
  level.player = player;
  return player;
}
//# sourceMappingURL=addPlayerToLevel.js.map