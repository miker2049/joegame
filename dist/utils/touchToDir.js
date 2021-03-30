"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.default = _default;

require("phaser");

var _joegameTypes = require("../joegameTypes");

function _default(coll) {
  if (coll.up) {
    return _joegameTypes.Dir.south;
  } else if (coll.down) {
    return _joegameTypes.Dir.north;
  } else if (coll.right) {
    return _joegameTypes.Dir.west;
  } else if (coll.left) {
    return _joegameTypes.Dir.east;
  }
}