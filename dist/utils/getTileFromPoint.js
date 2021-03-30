"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.default = _default;

require("phaser");

function _default(go, tilesize) {
  var x_ = Math.floor(go.x / tilesize) * tilesize;
  var y_ = Math.floor(go.y / tilesize) * tilesize;
  return {
    x: x_ / tilesize,
    y: y_ / tilesize
  };
}