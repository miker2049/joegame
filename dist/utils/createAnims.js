"use strict";

var _interopRequireDefault = require("@babel/runtime/helpers/interopRequireDefault");

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.default = createAnims;

require("phaser");

var _createAnimsFromSheet = _interopRequireDefault(require("./createAnimsFromSheet"));

var _wikiData = _interopRequireDefault(require("./wikiData"));

var _defaults = _interopRequireDefault(require("../defaults"));

/*
 * generate all animations from whatever textures happened to be loaded up
 * ASSUMES wikidata is loaded
 */
function createAnims(game) {
  for (var t in game.textures.list) {
    var sp = (0, _wikiData.default)(game).spritesheet.get(t);

    if (sp) {
      (0, _createAnimsFromSheet.default)(sp.key, sp.animLength || _defaults.default.animLength, game);
    }
  }
}