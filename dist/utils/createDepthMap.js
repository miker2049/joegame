"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.default = _default;

require("phaser");

var _getKeyNames = require("./getKeyNames");

function _default(game, mapjsonpath) {
  var mapjson = game.cache.json.get((0, _getKeyNames.getMapKeyNameRaw)(mapjsonpath));
  var layers = mapjson.layers.map(function (l, i) {
    return [l.name, i];
  });
  var map = new Map(layers);
  game.registry.remove('depthmap');
  game.registry.set('depthmap', map);
}
//# sourceMappingURL=createDepthMap.js.map