"use strict";

var _interopRequireDefault = require("@babel/runtime/helpers/interopRequireDefault");

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.default = _default;

require("phaser");

var _ensure = _interopRequireDefault(require("./ensure"));

function _default(path) {
  var transitions = [];

  for (var i = 1; i < path.length; i++) {
    var xDiff = (0, _ensure.default)(path[i].x) - (0, _ensure.default)(path[i - 1].x);
    var yDiff = (0, _ensure.default)(path[i].y) - (0, _ensure.default)(path[i - 1].y);
    transitions.push({
      x: xDiff,
      y: yDiff
    });
  }

  return transitions;
}
//# sourceMappingURL=createPathTransitions.js.map