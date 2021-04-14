"use strict";

var _interopRequireDefault = require("@babel/runtime/helpers/interopRequireDefault");

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.Level = void 0;

var _classCallCheck2 = _interopRequireDefault(require("@babel/runtime/helpers/classCallCheck"));

var _getKeyNames = require("./utils/getKeyNames");

var _createTilemap = _interopRequireDefault(require("./factories/createTilemap"));

var _createPathfinder = _interopRequireDefault(require("./factories/createPathfinder"));

var _MachineRegistry = require("./components/MachineRegistry");

var _Toner = _interopRequireDefault(require("./sound/Toner"));

var Level = function Level(game, mapjsonpath) {
  (0, _classCallCheck2.default)(this, Level);
  this.key = mapjsonpath ? mapjsonpath : 'empty';
  this.scene = game.scene.add((0, _getKeyNames.getSceneKeyName)(this.key), new Phaser.Scene((0, _getKeyNames.getSceneKeyName)(this.key)), true);
  this.npcs = this.scene.physics.add.group();
  this.platforms = this.scene.physics.add.group();
  this.map = (0, _createTilemap.default)(this.scene, this.key);
  this.pathfinder = (0, _createPathfinder.default)(this.map);
  this.machineRegistry = new _MachineRegistry.MachineRegistry();
  this.toner = new _Toner.default(game.sound.context);
};

exports.Level = Level;
//# sourceMappingURL=Level.js.map