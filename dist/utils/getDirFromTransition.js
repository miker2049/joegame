"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.default = _default;

var _joegameTypes = require("../joegameTypes");

function _default(pathTransition) {
  if (pathTransition.x === 1 && pathTransition.y === 0) {
    return _joegameTypes.Dir.east;
  } else if (pathTransition.x === -1 && pathTransition.y === 0) {
    return _joegameTypes.Dir.west;
  } else if (pathTransition.x === 0 && pathTransition.y === 1) {
    return _joegameTypes.Dir.south;
  } else if (pathTransition.x === 0 && pathTransition.y === -1) {
    return _joegameTypes.Dir.north;
  } else {
    return _joegameTypes.Dir.north;
  }
}
//# sourceMappingURL=getDirFromTransition.js.map