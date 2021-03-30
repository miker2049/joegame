"use strict";

var _interopRequireDefault = require("@babel/runtime/helpers/interopRequireDefault");

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.default = void 0;

var _classCallCheck2 = _interopRequireDefault(require("@babel/runtime/helpers/classCallCheck"));

var _createClass2 = _interopRequireDefault(require("@babel/runtime/helpers/createClass"));

var _assertThisInitialized2 = _interopRequireDefault(require("@babel/runtime/helpers/assertThisInitialized"));

var _inherits2 = _interopRequireDefault(require("@babel/runtime/helpers/inherits"));

var _possibleConstructorReturn2 = _interopRequireDefault(require("@babel/runtime/helpers/possibleConstructorReturn"));

var _getPrototypeOf2 = _interopRequireDefault(require("@babel/runtime/helpers/getPrototypeOf"));

var _defineProperty2 = _interopRequireDefault(require("@babel/runtime/helpers/defineProperty"));

require("phaser");

var _mdParse = _interopRequireDefault(require("../utils/mdParse"));

function _createSuper(Derived) { var hasNativeReflectConstruct = _isNativeReflectConstruct(); return function _createSuperInternal() { var Super = (0, _getPrototypeOf2.default)(Derived), result; if (hasNativeReflectConstruct) { var NewTarget = (0, _getPrototypeOf2.default)(this).constructor; result = Reflect.construct(Super, arguments, NewTarget); } else { result = Super.apply(this, arguments); } return (0, _possibleConstructorReturn2.default)(this, result); }; }

function _isNativeReflectConstruct() { if (typeof Reflect === "undefined" || !Reflect.construct) return false; if (Reflect.construct.sham) return false; if (typeof Proxy === "function") return true; try { Boolean.prototype.valueOf.call(Reflect.construct(Boolean, [], function () {})); return true; } catch (e) { return false; } }

var TextWindow = /*#__PURE__*/function (_Phaser$Scene) {
  (0, _inherits2.default)(TextWindow, _Phaser$Scene);

  var _super = _createSuper(TextWindow);

  function TextWindow() {
    var _this;

    (0, _classCallCheck2.default)(this, TextWindow);

    for (var _len = arguments.length, args = new Array(_len), _key = 0; _key < _len; _key++) {
      args[_key] = arguments[_key];
    }

    _this = _super.call.apply(_super, [this].concat(args));
    (0, _defineProperty2.default)((0, _assertThisInitialized2.default)(_this), "textBuff", '');
    return _this;
  }

  (0, _createClass2.default)(TextWindow, [{
    key: "init",
    value: function init(data) {
      var _data$width,
          _data$height,
          _ref,
          _this2 = this;

      var style = " list-style: none;\n             background-color: rgba(0,0,0,0.8);\n             overflow: hidden;\n             width:".concat((_data$width = data.width) !== null && _data$width !== void 0 ? _data$width : 250, "px;\n             margin: 0px;\n             padding: 1em;\n             color: white;\n             font-family: Retro Gaming;\n             height:").concat((_data$height = data.height) !== null && _data$height !== void 0 ? _data$height : 250, "px;\n             -ms-overflow-style:none;\n             scrollbar-width: none;");
      this.phaserDom = this.add.dom(data.x, data.y, 'div', (_ref = style + data.additionalStyle) !== null && _ref !== void 0 ? _ref : '').setOrigin(0, 0);

      if (data.text) {
        this.setMDText(data.text);
      }

      this.input.keyboard.enabled = true;
      this.phaserDom.alpha = 0;
      this.input.keyboard.on('keyup', function (event) {
        if (event.key === 'Enter' || event.key === 'Escape') {
          _this2.close();
        }
      });
    }
  }, {
    key: "open",
    value: function open() {
      this.tweens.add({
        targets: [this.phaserDom],
        alpha: 1,
        duration: 500
      });
    }
  }, {
    key: "close",
    value: function close() {
      this.input.keyboard.enabled = false;
      this.tweens.add({
        targets: [this.phaserDom],
        alpha: 0,
        duration: 500
      });
    }
  }, {
    key: "setMDText",
    value: function setMDText(text) {
      this.textBuff = text;
      this.updateHTML();
    }
  }, {
    key: "appendMDText",
    value: function appendMDText(text) {
      this.textBuff += text;
      this.updateHTML();
    }
  }, {
    key: "appendNewLineMDText",
    value: function appendNewLineMDText(text) {
      this.textBuff += "\n\n" + text;
      this.updateHTML();
    }
  }, {
    key: "updateHTML",
    value: function updateHTML() {
      this.phaserDom.setHTML((0, _mdParse.default)(this.textBuff));
    }
  }]);
  return TextWindow;
}(Phaser.Scene);

exports.default = TextWindow;