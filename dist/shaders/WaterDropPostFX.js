"use strict";

var _interopRequireDefault = require("@babel/runtime/helpers/interopRequireDefault");

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.WaterDropPostFX = void 0;

var _classCallCheck2 = _interopRequireDefault(require("@babel/runtime/helpers/classCallCheck"));

var _createClass2 = _interopRequireDefault(require("@babel/runtime/helpers/createClass"));

var _inherits2 = _interopRequireDefault(require("@babel/runtime/helpers/inherits"));

var _possibleConstructorReturn2 = _interopRequireDefault(require("@babel/runtime/helpers/possibleConstructorReturn"));

var _getPrototypeOf2 = _interopRequireDefault(require("@babel/runtime/helpers/getPrototypeOf"));

var _phaser = _interopRequireDefault(require("phaser"));

function _createSuper(Derived) { var hasNativeReflectConstruct = _isNativeReflectConstruct(); return function _createSuperInternal() { var Super = (0, _getPrototypeOf2.default)(Derived), result; if (hasNativeReflectConstruct) { var NewTarget = (0, _getPrototypeOf2.default)(this).constructor; result = Reflect.construct(Super, arguments, NewTarget); } else { result = Super.apply(this, arguments); } return (0, _possibleConstructorReturn2.default)(this, result); }; }

function _isNativeReflectConstruct() { if (typeof Reflect === "undefined" || !Reflect.construct) return false; if (Reflect.construct.sham) return false; if (typeof Proxy === "function") return true; try { Boolean.prototype.valueOf.call(Reflect.construct(Boolean, [], function () {})); return true; } catch (e) { return false; } }

var fragShader = "\n#define SHADER_NAME WATERDROP_FS\n\nprecision mediump float;\n\nuniform sampler2D uMainSampler;\nuniform sampler2D uMainSampler2;\n\nuniform int resizeMode;\nuniform float progress;\nuniform float fromRatio;\nuniform float toRatio;\n\nvarying vec2 outFragCoord;\n\n//  Transition specific\nuniform float amplitude;\nuniform float speed;\n\nvec4 getFromColor (vec2 uv)\n{\n    return texture2D(uMainSampler, uv);\n}\n\nvec4 getToColor (vec2 uv)\n{\n    if (resizeMode == 2)\n    {\n        //  cover\n        return texture2D(uMainSampler2, 0.5 + (vec2(uv.x, 1.0 - uv.y) - 0.5) * vec2(min(fromRatio / toRatio, 1.0), min((toRatio / fromRatio), 1.0)));\n    }\n    else if (resizeMode == 1)\n    {\n        //  contain\n        return texture2D(uMainSampler2, 0.5 + (vec2(uv.x, 1.0 - uv.y) - 0.5) * vec2(max(fromRatio / toRatio, 1.0), max((toRatio / fromRatio), 1.0)));\n    }\n    else\n    {\n        //  stretch\n        return texture2D(uMainSampler2, vec2(uv.x, 1.0 - uv.y));\n    }\n}\n\n// Transition Author: Pawe\u0142 P\u0142\xF3ciennik\n// Transition License: MIT\n\nvec4 transition (vec2 p)\n{\n    vec2 dir = p - vec2(0.5);\n\n    float dist = length(dir);\n\n    if (dist > progress)\n    {\n        return mix(getFromColor(p), getToColor(p), progress);\n    }\n    else\n    {\n        vec2 offset = dir * sin(dist * amplitude - progress * speed);\n\n        return mix(getFromColor(p + offset), getToColor(p), progress);\n    }\n}\n    \nvoid main ()\n{\n    vec2 uv = outFragCoord;\n\n    gl_FragColor = transition(uv);\n}\n";

var WaterDropPostFX = /*#__PURE__*/function (_Phaser$Renderer$WebG) {
  (0, _inherits2.default)(WaterDropPostFX, _Phaser$Renderer$WebG);

  var _super = _createSuper(WaterDropPostFX);

  /**
   * The progress of the transition effect. From 0 to 1.
   *
   * @type {number}
   * @memberof WaterDropPostFX
   */

  /**
   * The WebGL Texture being transitioned to.
   *
   * @type {WebGLTexture}
   * @memberof WaterDropPostFX
   */

  /**
   * The resize mode to be used for the target texture.
   * 
   * Can be either 0, 1 or 2, for stretch, contain and cover modes respectively.
   * 
   * The default is 'contain'.
   * 
   * Set via the `setResizeMode` method.
   *
   * @type {number}
   * @memberof WaterDropPostFX
   */

  /**
   * The ratio of the target texture (width / height).
   * 
   * This is set automatically in the `setTexture` method.
   *
   * @type {number}
   * @memberof WaterDropPostFX
   */

  /**
   * The amplitude of the effect.
   * 
   * This controls how many 'ripples' there are.
   * 
   * @type {number}
   * @memberof WaterDropPostFX
   */

  /**
   * The speed of the effect.
   * 
   * This controls how fast the ripples spread from the center.
   * 
   * @type {number}
   * @memberof WaterDropPostFX
   */

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
  function WaterDropPostFX(game) {
    var _this;

    (0, _classCallCheck2.default)(this, WaterDropPostFX);
    _this = _super.call(this, {
      game: game,
      name: 'WaterDropPostFX',
      fragShader: fragShader
    });
    _this.progress = 0;
    _this.resizeMode = 1;
    _this.toRatio = 0;
    _this.amplitude = 30;
    _this.speed = 30;
    return _this;
  }
  /**
   * @ignore
   */


  (0, _createClass2.default)(WaterDropPostFX, [{
    key: "onBoot",
    value: function onBoot() {
      this.setTexture();
    }
    /**
     * Set the resize mode of the target texture.
     * 
     * Can be either:
     * 
     * 0 - Stretch. The target texture is stretched to the size of the source texture.
     * 1 - Contain. The target texture is resized to fit the source texture. This is the default.
     * 2 - Cover. The target texture is resized to cover the source texture.
     * 
     * If the source and target textures are the same size, then use a resize mode of zero
     * for speed.
     *
     * @param {number} [mode=1] - The Resize Mode. Either 0, 1 or 2.
     * @returns {this}
     * @memberof WaterDropPostFX
     */

  }, {
    key: "setResizeMode",
    value: function setResizeMode() {
      var mode = arguments.length > 0 && arguments[0] !== undefined ? arguments[0] : 1;
      this.resizeMode = mode;
      return this;
    }
    /**
     * Set the texture to be transitioned to.
     * 
     * The texture must be already loaded and available from the Texture Manager.
     * 
     * You can optionally also set the resize mode. This can be either:
     * 
     * 0 - Stretch. The target texture is stretched to the size of the source texture.
     * 1 - Contain. The target texture is resized to fit the source texture. This is the default.
     * 2 - Cover. The target texture is resized to cover the source texture.
     * 
     * If the source and target textures are the same size, then use a resize mode of zero
     * for speed.
     *
     * @param {string} [texture='__DEFAULT'] - The key of the texture to use.
     * @param {number} [mode] - The Resize Mode. Either 0, 1 or 2.
     * @returns {this}
     * @memberof WaterDropPostFX
     */

  }, {
    key: "setTexture",
    value: function setTexture() {
      var texture = arguments.length > 0 && arguments[0] !== undefined ? arguments[0] : '__DEFAULT';
      var resizeMode = arguments.length > 1 ? arguments[1] : undefined;
      var phaserTexture = this.game.textures.getFrame(texture);

      if (!phaserTexture) {
        phaserTexture = this.game.textures.getFrame('__DEFAULT');
      }

      this.toRatio = phaserTexture.width / phaserTexture.height;
      this.targetTexture = phaserTexture.glTexture;

      if (resizeMode !== undefined) {
        this.resizeMode = resizeMode;
      }

      this.set1i('uMainSampler2', 1);
      this.set1f('toRatio', this.toRatio);
      return this;
    }
    /**
     * Sets the progress of this effect.
     * 
     * Progress is given as a value between 0 and 1.
     * 
     * You can call this method at any point, or modify the `progress` property
     * directly for the same result. This can be done via tweens, Scene transitions,
     * Loader progress updates or any other system.
     *
     * @param {number} [value=0] - The progress of the effect. A value between 0 and 1.
     * @returns {this}
     * @memberof WaterDropPostFX
     */

  }, {
    key: "setProgress",
    value: function setProgress() {
      var value = arguments.length > 0 && arguments[0] !== undefined ? arguments[0] : 0;
      this.progress = _phaser.default.Math.Clamp(value, 0, 1);
      return this;
    }
    /**
     * Sets the amplitude of the Water Drop effect.
     * 
     * This controls the number of ripples.
     * 
     * @param {number} [value=30] - The amplitude.
     * @returns {this}
     * @memberof WaterDropPostFX
     */

  }, {
    key: "setAmplitude",
    value: function setAmplitude() {
      var value = arguments.length > 0 && arguments[0] !== undefined ? arguments[0] : 30;
      this.amplitude = value;
      return this;
    }
    /**
     * Sets the speed of the Water Drop effect.
     * 
     * This controls how fast the ripples spread from the center.
     * 
     * @param {number} [value=30] - The amplitude.
     * @returns {this}
     * @memberof WaterDropPostFX
     */

  }, {
    key: "setSpeed",
    value: function setSpeed() {
      var value = arguments.length > 0 && arguments[0] !== undefined ? arguments[0] : 30;
      this.speed = value;
      return this;
    }
    /**
     * @ignore
     */

  }, {
    key: "onPreRender",
    value: function onPreRender() {
      this.set1f('progress', this.progress);
      this.set1i('resizeMode', this.resizeMode);
      this.set1f('amplitude', this.amplitude);
      this.set1f('speed', this.speed);
    }
    /**
     * @ignore
     */

  }, {
    key: "onDraw",
    value: function onDraw(renderTarget) {
      this.set1f('fromRatio', renderTarget.width / renderTarget.height);
      this.bindTexture(this.targetTexture, 1);
      this.bindAndDraw(renderTarget);
    }
  }]);
  return WaterDropPostFX;
}(_phaser.default.Renderer.WebGL.Pipelines.PostFXPipeline);

exports.WaterDropPostFX = WaterDropPostFX;
//# sourceMappingURL=WaterDropPostFX.js.map