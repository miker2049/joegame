"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.default = _default;

require("phaser");

function _default(go, tilesize) {
  if (go.body) {
    var x_ = Math.floor(go.body.center.x / tilesize) * tilesize;
    var y_ = Math.floor(go.body.center.y / tilesize) * tilesize;
    return {
      x: x_ / tilesize,
      y: y_ / tilesize + 1
    };
  } else {
    return {
      x: 0,
      y: 0
    };
  }
}