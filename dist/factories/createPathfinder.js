"use strict";

var _interopRequireWildcard = require("@babel/runtime/helpers/interopRequireWildcard");

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.default = _default;

var easystar = _interopRequireWildcard(require("easystarjs"));

function _default(map) {
  var finder = new easystar.js();
  var mapgrid = [];
  var acceptableTiles = new Set();

  var _loop = function _loop(y) {
    var col = [];

    var _loop2 = function _loop2(x) {
      //NOTE hardcoded layer to get collision info
      var tile_ = map.getTileAt(x, y, true, "Main"); //check if collide
      // if there is tile (that is not empty and not collides) and additionally none of the other tiles have collision,

      if (tile_) {
        var startIndex = map.layers.findIndex(function (l) {
          return l.name === 'Main';
        });
        var layers = map.layers.slice(startIndex); //ignore lower layers

        var abovetiles = layers.map(function (l) {
          var _map$getTileAt$proper;

          var ltile = (_map$getTileAt$proper = map.getTileAt(x, y, true, l.name).properties) === null || _map$getTileAt$proper === void 0 ? void 0 : _map$getTileAt$proper.collides;
          return ltile === true ? 1 : 0;
        });

        if (abovetiles.reduce(function (p, v) {
          return p + v;
        }) === 0) {
          col.push(1);
        } else {
          col.push(0);
        }
      } else {
        col.push(0);
      }
    };

    for (var x = 0; x < map.width; x++) {
      _loop2(x);
    }

    mapgrid.push(col);
  };

  for (var y = 0; y < map.height; y++) {
    _loop(y);
  }

  finder.setGrid(mapgrid);
  finder.setAcceptableTiles([1]);
  return finder;
}
//# sourceMappingURL=createPathfinder.js.map