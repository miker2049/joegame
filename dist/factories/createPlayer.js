"use strict";

var _interopRequireDefault = require("@babel/runtime/helpers/interopRequireDefault");

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.default = _default;

var _xstate = require("xstate");

var _createCharacter = _interopRequireDefault(require("./createCharacter"));

var _MoveMachine = require("../components/MoveMachine");

var _MoveController = _interopRequireDefault(require("../MoveController"));

function _default(name, x, y, level) {
  var char = (0, _createCharacter.default)(name, x, y, level);
  var moveMachine = (0, _xstate.interpret)((0, _MoveMachine.createMoveMachine)(char, level.map.tileWidth, level.pathfinder), {
    devTools: true,
    parent: (0, _xstate.interpret)((0, _xstate.Machine)({
      id: name + 'dummyparent'
    }))
  });
  level.machineRegisty.add('player_machine', moveMachine);
  new _MoveController.default(moveMachine, level.scene);
  return char;
}
//# sourceMappingURL=createPlayer.js.map