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

require("phaser");

function _createSuper(Derived) { var hasNativeReflectConstruct = _isNativeReflectConstruct(); return function _createSuperInternal() { var Super = (0, _getPrototypeOf2.default)(Derived), result; if (hasNativeReflectConstruct) { var NewTarget = (0, _getPrototypeOf2.default)(this).constructor; result = Reflect.construct(Super, arguments, NewTarget); } else { result = Super.apply(this, arguments); } return (0, _possibleConstructorReturn2.default)(this, result); }; }

function _isNativeReflectConstruct() { if (typeof Reflect === "undefined" || !Reflect.construct) return false; if (Reflect.construct.sham) return false; if (typeof Proxy === "function") return true; try { Boolean.prototype.valueOf.call(Reflect.construct(Boolean, [], function () {})); return true; } catch (e) { return false; } }

var RainfallPostFX = /*#__PURE__*/function (_Phaser$Renderer$WebG) {
  (0, _inherits2.default)(RainfallPostFX, _Phaser$Renderer$WebG);

  var _super = _createSuper(RainfallPostFX);

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
  function RainfallPostFX(game) {
    var _this;

    (0, _classCallCheck2.default)(this, RainfallPostFX);
    _this = _super.call(this, {
      game: game,
      name: 'rainfall',
      fragShader: "\nprecision mediump float;\nuniform float RAIN_DENSITY;\nuniform float BRIGHTNESS;        // raindrop brightness contrast\nconst float BLUR_LENGTH = 30.;        // max length of raindrop blured line\nconst float SPEED = 500.;\n\n#define rnd(p,s)   fract(sin( (p+(.01*s)) *12.9898) * 43758.5453)\nuniform vec2 iResolution;\n\nuniform sampler2D uMainSampler;\nuniform float iTime;\n\nvarying vec2 outFragCoord;\n\nvoid main()\n    {\n    vec2 R = iResolution.xy;\n    vec4 txt = texture2D(uMainSampler, outFragCoord);\n    vec2 U = gl_FragCoord.xy;\n    U -= .4;\n    U.x += 1.5;\n    vec4 O = txt;\n\n    float Ny = RAIN_DENSITY * R.y;            // number of drop per column\n    float LIM = floor(Ny);\n    for (float i=0.0; i<=0.; i++) {     // to deal with more than one drop per column\n        float y = floor( mod( rnd(U.x,2.*i)*R.y -SPEED*iTime, R.y) ); // drop altitude\n        if ( rnd(U.x,2.*i+1.) < (Ny-i) && abs( U.y - y) < BLUR_LENGTH*(U.x/R.x) )\n            O += vec4(0.,0.,BRIGHTNESS,0.); // / (U.x/R.x);  //  / (U.x/R.x); // variant: keep total drop brightness. attention: saturated on the left 5%\n    }\n\n\n   // O = sqrt(O);                              // gamma correction\n\n//    O.rgb += col.rgb * 0.5;\n\n    gl_FragColor = O;\n}"
    });

    _this.set1f('RAIN_DENSITY', 0.03);

    _this.set1f('BRIGHTNESS', 0.27); // this.set1f('slow', 0.5)
    // this.set1f('gray', 0.1)


    console.log('wjattt');
    return _this;
  }
  /**
   * @ignore
   */


  (0, _createClass2.default)(RainfallPostFX, [{
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
  return RainfallPostFX;
}(Phaser.Renderer.WebGL.Pipelines.PostFXPipeline);

exports.default = RainfallPostFX;