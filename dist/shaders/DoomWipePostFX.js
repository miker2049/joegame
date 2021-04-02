"use strict";

var _interopRequireDefault = require("@babel/runtime/helpers/interopRequireDefault");

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.DoomWipePostFX = void 0;

var _classCallCheck2 = _interopRequireDefault(require("@babel/runtime/helpers/classCallCheck"));

var _createClass2 = _interopRequireDefault(require("@babel/runtime/helpers/createClass"));

var _inherits2 = _interopRequireDefault(require("@babel/runtime/helpers/inherits"));

var _possibleConstructorReturn2 = _interopRequireDefault(require("@babel/runtime/helpers/possibleConstructorReturn"));

var _getPrototypeOf2 = _interopRequireDefault(require("@babel/runtime/helpers/getPrototypeOf"));

var _phaser = _interopRequireDefault(require("phaser"));

function _createSuper(Derived) { var hasNativeReflectConstruct = _isNativeReflectConstruct(); return function _createSuperInternal() { var Super = (0, _getPrototypeOf2.default)(Derived), result; if (hasNativeReflectConstruct) { var NewTarget = (0, _getPrototypeOf2.default)(this).constructor; result = Reflect.construct(Super, arguments, NewTarget); } else { result = Super.apply(this, arguments); } return (0, _possibleConstructorReturn2.default)(this, result); }; }

function _isNativeReflectConstruct() { if (typeof Reflect === "undefined" || !Reflect.construct) return false; if (Reflect.construct.sham) return false; if (typeof Proxy === "function") return true; try { Boolean.prototype.valueOf.call(Reflect.construct(Boolean, [], function () {})); return true; } catch (e) { return false; } }

var fragShader = "\n#define SHADER_NAME DOOMWIPE_FS\n\nprecision mediump float;\n\nuniform sampler2D uMainSampler;\nuniform sampler2D uMainSampler2;\n\nuniform int resizeMode;\nuniform float progress;\nuniform float fromRatio;\nuniform float toRatio;\n\nvarying vec2 outFragCoord;\n\n//  Transition specific\nuniform int bars;\nuniform float amplitude;\nuniform float noise;\nuniform float frequency;\nuniform float dripScale;\n\nvec4 getFromColor (vec2 uv)\n{\n    return texture2D(uMainSampler, uv);\n}\n\nvec4 getToColor (vec2 uv)\n{\n    if (resizeMode == 2)\n    {\n        //  cover\n        return texture2D(uMainSampler2, 0.5 + (vec2(uv.x, 1.0 - uv.y) - 0.5) * vec2(min(fromRatio / toRatio, 1.0), min((toRatio / fromRatio), 1.0)));\n    }\n    else if (resizeMode == 1)\n    {\n        //  contain\n        return texture2D(uMainSampler2, 0.5 + (vec2(uv.x, 1.0 - uv.y) - 0.5) * vec2(max(fromRatio / toRatio, 1.0), max((toRatio / fromRatio), 1.0)));\n    }\n    else\n    {\n        //  stretch\n        return texture2D(uMainSampler2, vec2(uv.x, 1.0 - uv.y));\n    }\n}\n\n// Transition Author: Zeh Fernando\n// Transition License: MIT\n\nfloat rand (int num)\n{\n    return fract(mod(float(num) * 67123.313, 12.0) * sin(float(num) * 10.3) * cos(float(num)));\n}\n  \nfloat wave (int num)\n{\n    float fn = float(num) * frequency * 0.1 * float(bars);\n    return cos(fn * 0.5) * cos(fn * 0.13) * sin((fn+10.0) * 0.3) / 2.0 + 0.5;\n}\n  \nfloat drip (int num)\n{\n    return sin(float(num) / float(bars - 1) * 3.141592) * dripScale;\n}\n  \nfloat pos (int num)\n{\n    return (noise == 0.0 ? wave(num) : mix(wave(num), rand(num), noise)) + (dripScale == 0.0 ? 0.0 : drip(num));\n}\n  \nvec4 transition (vec2 uv)\n{\n    int bar = int(uv.x * (float(bars)));\n    float scale = 1.0 + pos(bar) * amplitude;\n    float phase = progress * scale;\n    float posY = uv.y / vec2(1.0).y;\n\n    vec2 p;\n    vec4 c;\n\n    if (phase + posY < 1.0)\n    {\n        p = vec2(uv.x, uv.y + mix(0.0, vec2(1.0).y, phase)) / vec2(1.0).xy;\n        c = getFromColor(p);\n    }\n    else\n    {\n        p = uv.xy / vec2(1.0).xy;\n        c = getToColor(p);\n    }\n\n    // Finally, apply the color\n    return c;\n}\n\nvoid main ()\n{\n    vec2 uv = outFragCoord;\n\n    gl_FragColor = transition(uv);\n}\n";

var DoomWipePostFX = /*#__PURE__*/function (_Phaser$Renderer$WebG) {
  (0, _inherits2.default)(DoomWipePostFX, _Phaser$Renderer$WebG);

  var _super = _createSuper(DoomWipePostFX);

  /**
   * The progress of the transition effect. From 0 to 1.
   *
   * @type {number}
   * @memberof DoomWipePostFX
   */

  /**
   * The WebGL Texture being transitioned to.
   *
   * @type {WebGLTexture}
   * @memberof DoomWipePostFX
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
   * @memberof DoomWipePostFX
   */

  /**
   * The ratio of the target texture (width / height).
   * 
   * This is set automatically in the `setTexture` method.
   *
   * @type {number}
   * @memberof DoomWipePostFX
   */

  /**
   * The total number of bars / columns.
   * 
   * @type {number}
   * @memberof DoomWipePostFX
   */

  /**
   * Multiplier for the speed ratio.
   * 
   * 0 = No variation when going down.
   * Higher = Some elements go much faster.
   * 
   * @type {number}
   * @memberof DoomWipePostFX
   */

  /**
   * Further variations in speed.
   * 
   * 0 = No noise
   * 1 = Super noisy (ignore frequency)
   * 
   * @type {number}
   * @memberof DoomWipePostFX
   */

  /**
   * Horizontal speed variation.
   * 
   * The bigger the value, the shorter the waves.
   * 
   * @type {number}
   * @memberof DoomWipePostFX
   */

  /**
   * How much the bars seem to "run" from the middle of the screen
   * first (sticking to the sides).
   * 
   * 0 = No drip
   * 1 = Curved drip
   * 
   * @type {number}
   * @memberof DoomWipePostFX
   */

  /**
   * The Doom Wipe Post FX is an effect that allows you to transition
   * between two objects via an effect that looks like the effect used
   * in the classic FPS game Doom.
   * 
   * You can control the number of bars, amplitude, frequency and more.
   * 
   * The source image comes from the Game Object to which the FX is applied,
   * which can be any Game Object that supports post pipelines, such as a
   * Sprite, Rope or Layer. You can also transition Cameras and even entire
   * Scenes. Please see the examples and class docs for further details.
   * 
   * @param {Phaser.Game} game
   * @memberof DoomWipePostFX
   */
  function DoomWipePostFX(game) {
    var _this;

    (0, _classCallCheck2.default)(this, DoomWipePostFX);
    _this = _super.call(this, {
      game: game,
      name: 'DoomWipePostFX',
      fragShader: fragShader
    });
    _this.progress = 0;
    _this.resizeMode = 1;
    _this.toRatio = 0;
    _this.bars = 30;
    _this.amplitude = 2;
    _this.noise = 0.1;
    _this.frequency = 0.5;
    _this.dripScale = 0.5;
    return _this;
  }
  /**
   * @ignore
   */


  (0, _createClass2.default)(DoomWipePostFX, [{
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
     * @memberof DoomWipePostFX
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
     * @memberof DoomWipePostFX
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
     * @memberof DoomWipePostFX
     */

  }, {
    key: "setProgress",
    value: function setProgress() {
      var value = arguments.length > 0 && arguments[0] !== undefined ? arguments[0] : 0;
      this.progress = _phaser.default.Math.Clamp(value, 0, 1);
      return this;
    }
    /**
     * Sets the number of bars / columns that drop down.
     * 
     * @param {number} [value=30] - The number of bars / columns.
     * @returns {this}
     * @memberof DoomWipePostFX
     */

  }, {
    key: "setBars",
    value: function setBars() {
      var value = arguments.length > 0 && arguments[0] !== undefined ? arguments[0] : 30;
      this.bars = value;
      return this;
    }
    /**
     * Sets the multiplier for the drop speed ratio.
     * 
     * 0 = No variation when going down.
     * Higher = Some elements go much faster.
     * 
     * @param {number} [value=2] - The amplitude.
     * @returns {this}
     * @memberof DoomWipePostFX
     */

  }, {
    key: "setAmplitude",
    value: function setAmplitude() {
      var value = arguments.length > 0 && arguments[0] !== undefined ? arguments[0] : 2;
      this.amplitude = value;
      return this;
    }
    /**
     * Further variations in speed.
     * 
     * 0 = No noise
     * 1 = Super noisy (ignore frequency)
     * 
     * @param {number} [value=0.1] - The noise.
     * @returns {this}
     * @memberof DoomWipePostFX
     */

  }, {
    key: "setNoise",
    value: function setNoise() {
      var value = arguments.length > 0 && arguments[0] !== undefined ? arguments[0] : 0.1;
      this.noise = value;
      return this;
    }
    /**
     * Horizontal speed variation.
     * 
     * The bigger the value, the shorter the waves.
     * 
     * @param {number} [value=0.5] - The frequency.
     * @returns {this}
     * @memberof DoomWipePostFX
     */

  }, {
    key: "setFrequency",
    value: function setFrequency() {
      var value = arguments.length > 0 && arguments[0] !== undefined ? arguments[0] : 0.5;
      this.frequency = value;
      return this;
    }
    /**
     * How much the bars seem to "run" from the middle of the screen
     * first (sticking to the sides).
     * 
     * 0 = No drip
     * 1 = Curved drip
     * 
     * @param {number} [value=0.5] - The drip scale.
     * @returns {this}
     * @memberof DoomWipePostFX
     */

  }, {
    key: "setDripScale",
    value: function setDripScale() {
      var value = arguments.length > 0 && arguments[0] !== undefined ? arguments[0] : 0.5;
      this.dripScale = value;
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
      this.set1i('bars', this.bars);
      this.set1f('amplitude', this.amplitude);
      this.set1f('noise', this.noise);
      this.set1f('frequency', this.frequency);
      this.set1f('dripScale', this.dripScale);
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
  return DoomWipePostFX;
}(_phaser.default.Renderer.WebGL.Pipelines.PostFXPipeline);

exports.DoomWipePostFX = DoomWipePostFX;
//# sourceMappingURL=DoomWipePostFX.js.map