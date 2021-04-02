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

function _createSuper(Derived) { var hasNativeReflectConstruct = _isNativeReflectConstruct(); return function _createSuperInternal() { var Super = (0, _getPrototypeOf2.default)(Derived), result; if (hasNativeReflectConstruct) { var NewTarget = (0, _getPrototypeOf2.default)(this).constructor; result = Reflect.construct(Super, arguments, NewTarget); } else { result = Super.apply(this, arguments); } return (0, _possibleConstructorReturn2.default)(this, result); }; }

function _isNativeReflectConstruct() { if (typeof Reflect === "undefined" || !Reflect.construct) return false; if (Reflect.construct.sham) return false; if (typeof Proxy === "function") return true; try { Boolean.prototype.valueOf.call(Reflect.construct(Boolean, [], function () {})); return true; } catch (e) { return false; } }

var Blobs = /*#__PURE__*/function (_Phaser$Renderer$WebG) {
  (0, _inherits2.default)(Blobs, _Phaser$Renderer$WebG);

  var _super = _createSuper(Blobs);

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
  function Blobs(game) {
    var _this;

    (0, _classCallCheck2.default)(this, Blobs);
    _this = _super.call(this, {
      game: game,
      name: 'blobs',
      fragShader: "\n// Blobs by @paulofalcao\n\nprecision mediump float;\n\n\nuniform vec2 iResolution;\nuniform sampler2D uMainSampler;\nuniform float iTime;\nuniform float scale;\n\nvarying vec2 outFragCoord;\n\n#define time iTime\n\nfloat makePoint(float x,float y,float fx,float fy,float sx,float sy,float t){\n   float xx=x+sin(t*fx)*sx;\n   float yy=y+cos(t*fy)*sy;\n   return 1.0/sqrt(xx*xx+yy*yy);\n}\nvoid blobber(vec2 pos){\n  vec4 fragColor = texture2D(uMainSampler, outFragCoord);\n  vec2 fragCoord = gl_FragCoord.xy;\n   vec2 p=(fragCoord.xy/iResolution.x)*2.0-vec2(1.0,iResolution.y/iResolution.x);\n\n   p=p*8.0*scale;\n\n    vec2 pp = vec2(-2.0*pos.x,-2.0*pos.y) + (2.0 * gl_FragCoord.xy / iResolution.xy);\n   pp=pp*8.0*scale;\n   float x=pp.x;\n   float y=pp.y;\n// x += iResolution.x*0.125;\n// y -= 2.1;\n   float a=\n       makePoint(x,y,3.3,2.9,0.3,0.3,time);\n   a=a+makePoint(x,y,1.9,2.0,0.4,0.4,time);\n   a=a+makePoint(x,y,0.8,0.7,0.4,0.5,time);\n   a=a+makePoint(x,y,2.3,0.1,0.6,0.3,time);\n   a=a+makePoint(x,y,0.8,1.7,0.5,0.4,time);\n   a=a+makePoint(x,y,0.3,1.0,0.4,0.4,time);\n   a=a+makePoint(x,y,1.4,1.7,0.4,0.5,time);\n   a=a+makePoint(x,y,1.3,2.1,0.6,0.3,time);\n   a=a+makePoint(x,y,1.8,1.7,0.5,0.4,time);\n\n   float b=\n       makePoint(x,y,1.2,1.9,0.3,0.3,time);\n   b=b+makePoint(x,y,0.7,2.7,0.4,0.4,time);\n   b=b+makePoint(x,y,1.4,0.6,0.4,0.5,time);\n   b=b+makePoint(x,y,2.6,0.4,0.6,0.3,time);\n   b=b+makePoint(x,y,0.7,1.4,0.5,0.4,time);\n   b=b+makePoint(x,y,0.7,1.7,0.4,0.4,time);\n   b=b+makePoint(x,y,0.8,0.5,0.4,0.5,time);\n   b=b+makePoint(x,y,1.4,0.9,0.6,0.3,time);\n   b=b+makePoint(x,y,0.7,1.3,0.5,0.4,time);\n\n   float c=\n       makePoint(x,y,3.7,0.3,0.3,0.3,time);\n   c=c+makePoint(x,y,1.9,1.3,0.4,0.4,time);\n   c=c+makePoint(x,y,0.8,0.9,0.4,0.5,time);\n   c=c+makePoint(x,y,1.2,1.7,0.6,0.3,time);\n   c=c+makePoint(x,y,0.3,0.6,0.5,0.4,time);\n   c=c+makePoint(x,y,0.3,0.3,0.4,0.4,time);\n   c=c+makePoint(x,y,1.4,0.8,0.4,0.5,time);\n   c=c+makePoint(x,y,0.2,0.6,0.6,0.3,time);\n   c=c+makePoint(x,y,1.3,0.5,0.5,0.4,time);\n\n   vec3 d=vec3(a,b,c)/32.0;\n\n   fragColor += vec4(d.x,d.y,d.z,(d.x+d.y+d.z)/3.0);\n   gl_FragColor = fragColor;\n}\n\nvoid main() {\n  blobber(vec2(0.75,0.25));\n}\n"
    }); // this.set1f('RAIN_DENSITY', 0.03)
    // this.set1f('BRIGHTNESS', 0.27)
    // this.set1f('slow', 0.5)
    // this.set1f('gray', 0.1)

    console.log('wjattt');
    return _this;
  }
  /**
   * @ignore
   */


  (0, _createClass2.default)(Blobs, [{
    key: "onBoot",
    value: function onBoot() {
      // this.setTexture();
      this.set1f('scale', 1);
    }
  }, {
    key: "onPreRender",
    value: function onPreRender() {
      this.set1f('iTime', this.game.loop.time / 1000);
    }
  }, {
    key: "onDraw",
    value: function onDraw(renderTarget) {
      // this.game.phy
      this.set2f('iResolution', renderTarget.width, renderTarget.height);
      this.bindAndDraw(renderTarget);
    }
  }]);
  return Blobs;
}(_phaser.default.Renderer.WebGL.Pipelines.PostFXPipeline);

exports.default = Blobs;
//# sourceMappingURL=Blobs.js.map