"use strict";

var _interopRequireDefault = require("@babel/runtime/helpers/interopRequireDefault");

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.default = _default;

var _Character = _interopRequireDefault(require("../Character"));

var _wikiData = _interopRequireDefault(require("../utils/wikiData"));

var _defaults = _interopRequireDefault(require("../defaults"));

function _default(name, x, y, level) {
  var chardata = (0, _wikiData.default)(level.scene.game).character.get(name); // console.log(chardata)

  if (chardata) {
    var config = {
      level: level,
      x: x,
      y: y,
      name: name,
      texture: chardata.texture,
      anims: chardata.anims,
      speed: chardata.speed ? chardata.speed : _defaults.default.speed,
      scale: chardata.scale ? chardata.scale : _defaults.default.scale,
      dashDistance: chardata.dashDistance ? chardata.dashDistance : _defaults.default.dashDistance,
      body: chardata.body ? chardata.body : {
        offsetX: 0,
        offsetY: 0
      }
    };
    return new _Character.default(config);
  } else {
    throw new TypeError("couldnt get character ".concat(name));
  }
}
//# sourceMappingURL=createCharacter.js.map