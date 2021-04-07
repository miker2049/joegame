"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.default = _default;

require("phaser");

var _getKeyNames = require("../utils/getKeyNames");

function _createForOfIteratorHelper(o, allowArrayLike) { var it; if (typeof Symbol === "undefined" || o[Symbol.iterator] == null) { if (Array.isArray(o) || (it = _unsupportedIterableToArray(o)) || allowArrayLike && o && typeof o.length === "number") { if (it) o = it; var i = 0; var F = function F() {}; return { s: F, n: function n() { if (i >= o.length) return { done: true }; return { done: false, value: o[i++] }; }, e: function e(_e) { throw _e; }, f: F }; } throw new TypeError("Invalid attempt to iterate non-iterable instance.\nIn order to be iterable, non-array objects must have a [Symbol.iterator]() method."); } var normalCompletion = true, didErr = false, err; return { s: function s() { it = o[Symbol.iterator](); }, n: function n() { var step = it.next(); normalCompletion = step.done; return step; }, e: function e(_e2) { didErr = true; err = _e2; }, f: function f() { try { if (!normalCompletion && it.return != null) it.return(); } finally { if (didErr) throw err; } } }; }

function _unsupportedIterableToArray(o, minLen) { if (!o) return; if (typeof o === "string") return _arrayLikeToArray(o, minLen); var n = Object.prototype.toString.call(o).slice(8, -1); if (n === "Object" && o.constructor) n = o.constructor.name; if (n === "Map" || n === "Set") return Array.from(o); if (n === "Arguments" || /^(?:Ui|I)nt(?:8|16|32)(?:Clamped)?Array$/.test(n)) return _arrayLikeToArray(o, minLen); }

function _arrayLikeToArray(arr, len) { if (len == null || len > arr.length) len = arr.length; for (var i = 0, arr2 = new Array(len); i < len; i++) { arr2[i] = arr[i]; } return arr2; }

function _default(scene, mapjsonpath, offsetX, offsetY) {
  var tilemap = scene.make.tilemap({
    key: (0, _getKeyNames.getMapKeyName)(mapjsonpath)
  }); // const depthmap = scene.game.registry.get('depthmap')
  //initialize tilesets, and also leave a reference to them so they can easily be used in making the layers

  var _iterator = _createForOfIteratorHelper(tilemap.tilesets),
      _step;

  try {
    for (_iterator.s(); !(_step = _iterator.n()).done;) {
      var tileset = _step.value;

      // Note that here, to keep things simpler, every tileset is preloaded with the name of the filename itself, so two `tileset.name`s
      if (tileset.total > 1) {
        tilemap.addTilesetImage(tileset.name, tileset.name, tileset.tileWidth, tileset.tileHeight, tileset.tileMargin, tileset.tileSpacing);
      }
    } // init all our layers...

  } catch (err) {
    _iterator.e(err);
  } finally {
    _iterator.f();
  }

  tilemap.layers.forEach(function (l, i) {
    tilemap.createLayer(l.name, tilemap.tilesets, offsetX || 0, offsetY || 0);
  });
  tilemap.createBlankLayer('highlight', tilemap.tilesets).setVisible(true); // collision for map

  tilemap.layers.forEach(function (l) {
    l.tilemapLayer.setCollisionByProperty({
      collides: true
    });
  });
  return tilemap;
}
//# sourceMappingURL=createTilemap.js.map