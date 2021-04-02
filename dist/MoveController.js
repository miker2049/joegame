"use strict";

var _interopRequireDefault = require("@babel/runtime/helpers/interopRequireDefault");

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.default = void 0;

var _toConsumableArray2 = _interopRequireDefault(require("@babel/runtime/helpers/toConsumableArray"));

var _classCallCheck2 = _interopRequireDefault(require("@babel/runtime/helpers/classCallCheck"));

var _createClass2 = _interopRequireDefault(require("@babel/runtime/helpers/createClass"));

var _defineProperty2 = _interopRequireDefault(require("@babel/runtime/helpers/defineProperty"));

require("phaser");

var _joegameTypes = require("./joegameTypes");

// import { ICharacterMoveMachine } from './ICharacter'
var Action;

(function (Action) {
  Action[Action["keydown"] = 0] = "keydown";
  Action[Action["keyup"] = 1] = "keyup";
})(Action || (Action = {}));

var MoveController = /*#__PURE__*/function () {
  function MoveController(moveMachine, scene) {
    var _this = this;

    (0, _classCallCheck2.default)(this, MoveController);
    (0, _defineProperty2.default)(this, "held", new Set());
    (0, _defineProperty2.default)(this, "gameplayKeyDown", function (event) {
      if (event.repeat === true) return;
      if (_this.shiftMapping[event.key] === undefined && _this.mapping[event.key] === undefined) return;

      if (event.shiftKey && _this.shiftMapping[event.key]) {
        _this.shiftMapping[event.key]();

        return;
      }

      _this.held.add(event.key);

      var ordered = (0, _toConsumableArray2.default)(_this.held);
      var lastheld = ordered[ordered.length - 1];

      if (_this.mapping[lastheld]) {
        _this.mapping[lastheld]();
      }
    });
    (0, _defineProperty2.default)(this, "gameplayKeyUp", function (event) {
      if (event.repeat === true) return;

      _this.held.delete(event.key);

      var ordered = (0, _toConsumableArray2.default)(_this.held);
      var lastheld = ordered[ordered.length - 1];

      if (lastheld != undefined && _this.mapping[lastheld]) {
        // this.char.body.setVelocity(0,0);
        // this.char.alignOnTile();
        _this.mapping[lastheld]();
      } else {
        // this.char.stopMove();
        _this.moveMachine.send('STOP');
      }
    });
    (0, _defineProperty2.default)(this, "gameplayMouseDown", function () {
      var point = _this.scene.input.activePointer;

      if (point.leftButtonDown()) {
        _this.moveMachine.send('MOVE_ON_PATH', {
          point: {
            x: point.worldX,
            y: point.worldY
          }
        });
      }
    });
    (0, _defineProperty2.default)(this, "mapping", {
      ArrowLeft: function ArrowLeft() {
        _this.moveMachine.send('MOVE', {
          dir: _joegameTypes.Dir.west
        });
      },
      ArrowRight: function ArrowRight() {
        // this.char.move(Dir.east);
        _this.moveMachine.send('MOVE', {
          dir: _joegameTypes.Dir.east
        });
      },
      ArrowUp: function ArrowUp() {
        // this.char.move(Dir.north);
        _this.moveMachine.send('MOVE', {
          dir: _joegameTypes.Dir.north
        });
      },
      ArrowDown: function ArrowDown() {
        // this.char.move(Dir.south)
        _this.moveMachine.send('MOVE', {
          dir: _joegameTypes.Dir.south
        });
      },
      a: function a() {// this.char.move(Dir.south)
        // console.log(this.char.align());
      },
      // "d": this.mapping["ArrowRight"],
      // "w": this.mapping["ArrowUp"],
      // "s": this.mapping["ArrowDown"],
      // "h": this.mapping["ArrowLeft"],
      // "l": this.mapping["ArrowRight"],
      // "k": this.mapping["ArrowUp"],
      // "j": this.mapping["ArrowDown"],
      y: function y() {},
      u: function u() {},
      ' ': function _() {// this.scene.initDialogue();
      }
    });
    (0, _defineProperty2.default)(this, "shiftMapping", {
      ArrowLeft: function ArrowLeft() {
        _this.moveMachine.send('DASH', {
          dir: _joegameTypes.Dir.west
        });
      },
      ArrowRight: function ArrowRight() {
        _this.moveMachine.send('DASH', {
          dir: _joegameTypes.Dir.east
        });
      },
      ArrowUp: function ArrowUp() {
        _this.moveMachine.send('DASH', {
          dir: _joegameTypes.Dir.north
        });
      },
      ArrowDown: function ArrowDown() {
        _this.moveMachine.send('DASH', {
          dir: _joegameTypes.Dir.south
        });
      },
      a: function a() {},
      d: function d() {},
      w: function w() {},
      s: function s() {},
      t: function t() {},
      ' ': function _() {}
    });
    this.moveMachine = moveMachine;
    this.scene = scene;
    this.setGameplayControl();
  }

  (0, _createClass2.default)(MoveController, [{
    key: "setGameplayControl",
    value: function setGameplayControl() {
      var _this2 = this;

      this.scene.input.keyboard.on('keydown', this.gameplayKeyDown);
      this.scene.input.keyboard.on('keyup', this.gameplayKeyUp);
      this.scene.input.on('pointerdown', this.gameplayMouseDown);
      this.scene.input.on(Phaser.Input.Events.POINTER_WHEEL, function (_ref) {
        var deltaY = _ref.deltaY;

        if (deltaY < 0) {
          var am = _this2.scene.cameras.main.zoom + 0.8;

          _this2.scene.cameras.main.zoomTo(am > 10 ? 10 : am);
        } else {
          var _am = _this2.scene.cameras.main.zoom - 0.8;

          _this2.scene.cameras.main.zoomTo(_am < 0.2 ? 0.2 : _am);
        }
      });
    }
  }]);
  return MoveController;
}();

exports.default = MoveController;
//# sourceMappingURL=MoveController.js.map