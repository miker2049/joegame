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

function _createSuper(Derived) { var hasNativeReflectConstruct = _isNativeReflectConstruct(); return function _createSuperInternal() { var Super = (0, _getPrototypeOf2.default)(Derived), result; if (hasNativeReflectConstruct) { var NewTarget = (0, _getPrototypeOf2.default)(this).constructor; result = Reflect.construct(Super, arguments, NewTarget); } else { result = Super.apply(this, arguments); } return (0, _possibleConstructorReturn2.default)(this, result); }; }

function _isNativeReflectConstruct() { if (typeof Reflect === "undefined" || !Reflect.construct) return false; if (Reflect.construct.sham) return false; if (typeof Proxy === "function") return true; try { Boolean.prototype.valueOf.call(Reflect.construct(Boolean, [], function () {})); return true; } catch (e) { return false; } }

var fragShader = "\n#define SHADER_NAME PLASMA_FS\n\nprecision mediump float;\n\nuniform sampler2D uMainSampler;\nuniform float uTime;\nuniform vec2 uResolution;\n\nvarying vec2 outTexCoord;\n\nvoid main()\n{\n    vec2 p = -1.0 + 2.0 * gl_FragCoord.xy / uResolution.xy;\n\n    float x = p.x;\n    float y = p.y;\n    float mov0 = x+y+cos(sin(uTime)*2.0)*100.+sin(x/100.)*1000.;\n    float mov1 = y / 0.9 +  uTime;\n    float mov2 = x / 0.2;\n    float c1 = abs(sin(mov1+uTime)/2.+mov2/2.-mov1-mov2+uTime);\n    float c2 = abs(sin(c1+sin(mov0/1000.+uTime)+sin(y/40.+uTime)+sin((x+y)/100.)*3.));\n    float c3 = abs(sin(c2+cos(mov1+mov2+c2)+cos(mov2)+sin(x/1000.)));\n\n    vec4 pixel = texture2D(uMainSampler, outTexCoord);\n\n    gl_FragColor = pixel * vec4(c1, c2, c3, 1);\n}\n";

var PlasmaPostFX = /*#__PURE__*/function (_Phaser$Renderer$WebG) {
  (0, _inherits2.default)(PlasmaPostFX, _Phaser$Renderer$WebG);

  var _super = _createSuper(PlasmaPostFX);

  function PlasmaPostFX(game) {
    (0, _classCallCheck2.default)(this, PlasmaPostFX);
    return _super.call(this, {
      game: game,
      name: 'PlasmaPostFX',
      fragShader: fragShader,
      uniforms: ['uMainSampler', 'uTime', 'uResolution']
    });
  }

  (0, _createClass2.default)(PlasmaPostFX, [{
    key: "onPreRender",
    value: function onPreRender() {
      this.set1f('uTime', this.game.loop.time / 1000);
    }
  }, {
    key: "onDraw",
    value: function onDraw(renderTarget) {
      this.set2f('uResolution', renderTarget.width, renderTarget.height);
      this.bindAndDraw(renderTarget);
    }
  }]);
  return PlasmaPostFX;
}(Phaser.Renderer.WebGL.Pipelines.PostFXPipeline);

exports.default = PlasmaPostFX;