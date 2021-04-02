"use strict";

var _interopRequireDefault = require("@babel/runtime/helpers/interopRequireDefault");

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.default = void 0;

var _classCallCheck2 = _interopRequireDefault(require("@babel/runtime/helpers/classCallCheck"));

var _createClass2 = _interopRequireDefault(require("@babel/runtime/helpers/createClass"));

var _inherits2 = _interopRequireDefault(require("@babel/runtime/helpers/inherits"));

var _possibleConstructorReturn2 = _interopRequireDefault(require("@babel/runtime/helpers/possibleConstructorReturn"));

var _getPrototypeOf2 = _interopRequireDefault(require("@babel/runtime/helpers/getPrototypeOf"));

var _phaser = _interopRequireDefault(require("phaser"));

var _clouds = _interopRequireDefault(require("./clouds.glsl"));

function _createSuper(Derived) { var hasNativeReflectConstruct = _isNativeReflectConstruct(); return function _createSuperInternal() { var Super = (0, _getPrototypeOf2.default)(Derived), result; if (hasNativeReflectConstruct) { var NewTarget = (0, _getPrototypeOf2.default)(this).constructor; result = Reflect.construct(Super, arguments, NewTarget); } else { result = Super.apply(this, arguments); } return (0, _possibleConstructorReturn2.default)(this, result); }; }

function _isNativeReflectConstruct() { if (typeof Reflect === "undefined" || !Reflect.construct) return false; if (Reflect.construct.sham) return false; if (typeof Proxy === "function") return true; try { Boolean.prototype.valueOf.call(Reflect.construct(Boolean, [], function () {})); return true; } catch (e) { return false; } }

var fragShader = _clouds.default;

var Clouds = /*#__PURE__*/function (_Phaser$Renderer$WebG) {
  (0, _inherits2.default)(Clouds, _Phaser$Renderer$WebG);

  var _super = _createSuper(Clouds);

  /**
   * The Water Drop Post FX is an effect that allows you to transition
   * between two objects via an effect that looks like water rippling
   * out from the surface. You can control the amplitude and speed of
   * the ripple.
   *
   * The source image comes from the Game Object to which the FX is applied,
   * which can be any Game Object that supports post pipelines, such as a
   * Sprite, Rope or Layer. You can also transition Cameras and even entire
   * Scenes. Please see the examples and class docs for further details.
   *
   * @param {Phaser.Game} game
   * @memberof WaterDropPostFX
   */
  function Clouds(game) {
    (0, _classCallCheck2.default)(this, Clouds);
    return _super.call(this, {
      game: game,
      name: 'clouds',
      fragShader: fragShader
    }); // this.set1f('RAIN_DENSITY', 0.03)
    // this.set1f('BRIGHTNESS', 0.27)
    // this.set1f('slow', 0.5)
    // this.set1f('gray', 0.1)
  }
  /**
   * @ignore
   */


  (0, _createClass2.default)(Clouds, [{
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
      // this.set1f('fromRatio', renderTarget.width / renderTarget.height);
      this.set2f('iResolution', renderTarget.width, renderTarget.height); // console.log(Math.floor(0.003 * renderTarget.height))
      // this.bindTexture(this.targetTexture, 1);

      this.bindAndDraw(renderTarget);
    }
  }]);
  return Clouds;
}(_phaser.default.Renderer.WebGL.Pipelines.PostFXPipeline);

exports.default = Clouds;
//# sourceMappingURL=clouds.js.map