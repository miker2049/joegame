"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.default = _default;

require("phaser");

var _Level = require("../Level");

function _default(game, mapjsonpath) {
  return new _Level.Level(game, mapjsonpath);
}