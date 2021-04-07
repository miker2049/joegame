"use strict";

var _interopRequireDefault = require("@babel/runtime/helpers/interopRequireDefault");

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.default = void 0;

var _regenerator = _interopRequireDefault(require("@babel/runtime/regenerator"));

var _asyncToGenerator2 = _interopRequireDefault(require("@babel/runtime/helpers/asyncToGenerator"));

var _classCallCheck2 = _interopRequireDefault(require("@babel/runtime/helpers/classCallCheck"));

var _createClass2 = _interopRequireDefault(require("@babel/runtime/helpers/createClass"));

var _inherits2 = _interopRequireDefault(require("@babel/runtime/helpers/inherits"));

var _possibleConstructorReturn2 = _interopRequireDefault(require("@babel/runtime/helpers/possibleConstructorReturn"));

var _getPrototypeOf2 = _interopRequireDefault(require("@babel/runtime/helpers/getPrototypeOf"));

require("phaser");

var _typewriteText = _interopRequireDefault(require("../utils/typewriteText"));

function _createSuper(Derived) { var hasNativeReflectConstruct = _isNativeReflectConstruct(); return function _createSuperInternal() { var Super = (0, _getPrototypeOf2.default)(Derived), result; if (hasNativeReflectConstruct) { var NewTarget = (0, _getPrototypeOf2.default)(this).constructor; result = Reflect.construct(Super, arguments, NewTarget); } else { result = Super.apply(this, arguments); } return (0, _possibleConstructorReturn2.default)(this, result); }; }

function _isNativeReflectConstruct() { if (typeof Reflect === "undefined" || !Reflect.construct) return false; if (Reflect.construct.sham) return false; if (typeof Proxy === "function") return true; try { Boolean.prototype.valueOf.call(Reflect.construct(Boolean, [], function () {})); return true; } catch (e) { return false; } }

var BOXALPHA = 0.7;

var VoxBox = /*#__PURE__*/function (_Phaser$GameObjects$T) {
  (0, _inherits2.default)(VoxBox, _Phaser$GameObjects$T);

  var _super = _createSuper(VoxBox);

  function VoxBox(level) {
    var _this;

    (0, _classCallCheck2.default)(this, VoxBox);
    _this = _super.call(this, level.scene, 0, 4, '', {
      // fontFamily: 'Retro Gaming',
      fontSize: '12px',
      wordWrap: {
        width: level.map.tileWidth * 7
      },
      padding: {
        x: 2,
        y: 2
      },
      fixedWidth: level.map.tileWidth * 7,
      fixedHeight: level.map.tileWidth * 3.5
    });

    _this.setWordWrapCallback(function (str) {
      var wrapped = _this.basicWordWrap(str, _this.context, level.map.tileWidth * 7);

      var splitt = wrapped.split('\n');
      return splitt.slice(-5);
    });

    _this.textbuff = '';

    _this.setAlpha(BOXALPHA);

    _this.setBackgroundColor('black');

    _this.setOrigin(0.5, 1);

    _this.setScale(1 / (level.scene.cameras.default.zoom * 2)); // this.setMaxLines


    return _this;
  }

  (0, _createClass2.default)(VoxBox, [{
    key: "speak",
    value: function () {
      var _speak = (0, _asyncToGenerator2.default)( /*#__PURE__*/_regenerator.default.mark(function _callee(str, speed) {
        var _this2 = this;

        return _regenerator.default.wrap(function _callee$(_context) {
          while (1) {
            switch (_context.prev = _context.next) {
              case 0:
                this.setMDText('');
                this.open();
                _context.next = 4;
                return (0, _typewriteText.default)(str, this, this.scene, speed);

              case 4:
                // this.setText(str)
                this.scene.time.addEvent({
                  delay: 1500,
                  callback: function callback() {
                    _this2.close();
                  }
                });

              case 5:
              case "end":
                return _context.stop();
            }
          }
        }, _callee, this);
      }));

      function speak(_x, _x2) {
        return _speak.apply(this, arguments);
      }

      return speak;
    }()
  }, {
    key: "open",
    value: function open() {
      this.scene.tweens.add({
        targets: [this],
        alpha: BOXALPHA,
        duration: 500
      });
    }
  }, {
    key: "close",
    value: function close() {
      this.scene.tweens.add({
        targets: [this],
        alpha: 0,
        duration: 500
      });
    } //TODO stop lying about this

  }, {
    key: "setMDText",
    value: function setMDText(text) {
      this.textbuff = text;
      this.updateVoxtext();
    }
  }, {
    key: "appendMDText",
    value: function appendMDText(text) {
      this.textbuff += text;
      this.updateVoxtext();
    }
  }, {
    key: "appendNewLineMDText",
    value: function appendNewLineMDText(text) {
      this.textbuff += "\n\n" + text;
      this.updateVoxtext();
    }
  }, {
    key: "updateVoxtext",
    value: function updateVoxtext() {
      this.setText(this.textbuff);
      this.updateText();
    }
  }]);
  return VoxBox;
}(Phaser.GameObjects.Text);

exports.default = VoxBox;
//# sourceMappingURL=VoxBox.js.map