"use strict";

var _interopRequireDefault = require("@babel/runtime/helpers/interopRequireDefault");

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.default = _default;

require("phaser");

var _shuffleArr = _interopRequireDefault(require("./shuffleArr"));

function _default(map, amount, start, bounds) {
  var boundss = bounds !== null && bounds !== void 0 ? bounds : {
    x: 0,
    y: 0,
    width: map.width,
    height: map.height
  };
  var rect = new Phaser.Geom.Rectangle(boundss.x, boundss.y, boundss.width, boundss.height);
  var filtertiles = map.getTilesWithinShape(rect);
  var tiles = [];

  for (var i = 0; i < (amount !== null && amount !== void 0 ? amount : 3) + 1; i++) {
    tiles.push(getRandomValidTile(filtertiles));
  }

  return tiles.map(function (item, i, arr) {
    return {
      x: item.x,
      y: item.y
    };
  });
}

function getRandomValidTile(tiles) {
  var tile = (0, _shuffleArr.default)(tiles)[0];
  return tile;
}