"use strict";

var _interopRequireDefault = require("@babel/runtime/helpers/interopRequireDefault");

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.default = _default;

var _NPCMachine = require("../components/NPCMachine");

var _createCharacter = _interopRequireDefault(require("../factories/createCharacter"));

function _default(name, interestSets, level) {
  var char = (0, _createCharacter.default)(name, interestSets[0].x, interestSets[0].y, level);
  var mach = (0, _NPCMachine.createNPCMachine)(char, level.map.tileWidth, level.pathfinder, interestSets);
  return [char, mach];
}
//# sourceMappingURL=createNPC.js.map