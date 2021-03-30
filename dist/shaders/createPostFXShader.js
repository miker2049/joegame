"use strict";

var _interopRequireDefault = require("@babel/runtime/helpers/interopRequireDefault");

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.default = _default;

var _classCallCheck2 = _interopRequireDefault(require("@babel/runtime/helpers/classCallCheck"));

var _createClass2 = _interopRequireDefault(require("@babel/runtime/helpers/createClass"));

var _inherits2 = _interopRequireDefault(require("@babel/runtime/helpers/inherits"));

var _possibleConstructorReturn2 = _interopRequireDefault(require("@babel/runtime/helpers/possibleConstructorReturn"));

var _getPrototypeOf2 = _interopRequireDefault(require("@babel/runtime/helpers/getPrototypeOf"));

require("phaser");

function _createSuper(Derived) { var hasNativeReflectConstruct = _isNativeReflectConstruct(); return function _createSuperInternal() { var Super = (0, _getPrototypeOf2.default)(Derived), result; if (hasNativeReflectConstruct) { var NewTarget = (0, _getPrototypeOf2.default)(this).constructor; result = Reflect.construct(Super, arguments, NewTarget); } else { result = Super.apply(this, arguments); } return (0, _possibleConstructorReturn2.default)(this, result); }; }

function _isNativeReflectConstruct() { if (typeof Reflect === "undefined" || !Reflect.construct) return false; if (Reflect.construct.sham) return false; if (typeof Proxy === "function") return true; try { Boolean.prototype.valueOf.call(Reflect.construct(Boolean, [], function () {})); return true; } catch (e) { return false; } }

/*
 * Aiming to be used right now to
 */
function _default(game, name, fragShader) {
  return /*#__PURE__*/function (_Phaser$Renderer$WebG) {
    (0, _inherits2.default)(_class, _Phaser$Renderer$WebG);

    var _super = _createSuper(_class);

    function _class(game) {
      (0, _classCallCheck2.default)(this, _class);
      return _super.call(this, {
        game: game,
        name: name,
        fragShader: fragShader
      });
    }

    (0, _createClass2.default)(_class, [{
      key: "onBoot",
      value: function onBoot() {// this.setTexture();
      }
    }, {
      key: "onPreRender",
      value: function onPreRender() {
        this.set1f('iTime', this.game.loop.time / 1000);
      }
    }, {
      key: "onDraw",
      value: function onDraw(renderTarget) {
        this.set2f('iResolution', renderTarget.width, renderTarget.height);
        this.bindAndDraw(renderTarget);
      }
    }]);
    return _class;
  }(Phaser.Renderer.WebGL.Pipelines.PostFXPipeline);
}