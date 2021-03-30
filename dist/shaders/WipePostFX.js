"use strict";

var _interopRequireDefault = require("@babel/runtime/helpers/interopRequireDefault");

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.WipePostFX = void 0;

var _classCallCheck2 = _interopRequireDefault(require("@babel/runtime/helpers/classCallCheck"));

var _createClass2 = _interopRequireDefault(require("@babel/runtime/helpers/createClass"));

var _inherits2 = _interopRequireDefault(require("@babel/runtime/helpers/inherits"));

var _possibleConstructorReturn2 = _interopRequireDefault(require("@babel/runtime/helpers/possibleConstructorReturn"));

var _getPrototypeOf2 = _interopRequireDefault(require("@babel/runtime/helpers/getPrototypeOf"));

var _phaser = _interopRequireDefault(require("phaser"));

function _createSuper(Derived) { var hasNativeReflectConstruct = _isNativeReflectConstruct(); return function _createSuperInternal() { var Super = (0, _getPrototypeOf2.default)(Derived), result; if (hasNativeReflectConstruct) { var NewTarget = (0, _getPrototypeOf2.default)(this).constructor; result = Reflect.construct(Super, arguments, NewTarget); } else { result = Super.apply(this, arguments); } return (0, _possibleConstructorReturn2.default)(this, result); }; }

function _isNativeReflectConstruct() { if (typeof Reflect === "undefined" || !Reflect.construct) return false; if (Reflect.construct.sham) return false; if (typeof Proxy === "function") return true; try { Boolean.prototype.valueOf.call(Reflect.construct(Boolean, [], function () {})); return true; } catch (e) { return false; } }

var wipeFragShader = "\n#define SHADER_NAME WIPE_FS\n\nprecision mediump float;\n\nuniform sampler2D uMainSampler;\nuniform sampler2D uMainSampler2;\nuniform vec2 uResolution;\nuniform vec4 uInput;\nuniform float uReveal;\n\nvoid main ()\n{\n    vec2 uv = gl_FragCoord.xy / uResolution.xy;\n\n    vec4 color0;\n    vec4 color1;\n            \n    if (uReveal == 0.0)\n    {\n        color0 = texture2D(uMainSampler, uv);\n        color1 = texture2D(uMainSampler2, vec2(uv.x, 1.0 - uv.y));\n    }\n    else\n    {\n        color0 = texture2D(uMainSampler2, vec2(uv.x, 1.0 - uv.y));\n        color1 = texture2D(uMainSampler, uv);\n    }\n\n    float distance = uInput.x;\n    float width = uInput.y;\n    float direction = uInput.z;\n    float axis = uv.x;\n\n    if (uInput.w == 1.0)\n    {\n        axis = uv.y;\n    }\n\n    float adjust = mix(width, -width, distance);\n    \n    float value = smoothstep(distance - width, distance + width, abs(direction - axis) + adjust);\n\n    gl_FragColor = mix(color1, color0, value);\n}\n";

var WipePostFX = /*#__PURE__*/function (_Phaser$Renderer$WebG) {
  (0, _inherits2.default)(WipePostFX, _Phaser$Renderer$WebG);

  var _super = _createSuper(WipePostFX);

  /**
   * The progress of the wipe effect. From 0 to 1.
   *
   * @type {number}
   * @memberof WipePostFX
   */

  /**
   * The width of the wipe effect.
   * 
   * Given as a percentage of the overall texture width, between 0 and 1.
   *
   * @type {number}
   * @memberof WipePostFX
   * @default 0.1
   */

  /**
   * The direction of the effect.
   *
   * @type {number}
   * @memberof WipePostFX
   * @private
   */

  /**
   * The axis of the effect.
   *
   * @type {number}
   * @memberof WipePostFX
   * @private
   */

  /**
   * Is this a reveal (1) or a wipe (0) ?
   *
   * @type {number}
   * @memberof WipePostFX
   * @private
   */

  /**
   * The WebGL Texture being 'wiped to'.
   *
   * @type {WebGLTexture}
   * @memberof WipePostFX
   */

  /**
   * Creates an instance of WipePostFX.
   * 
   * @param {Phaser.Game} game
   * @memberof WipePostFX
   */
  function WipePostFX(game) {
    var _this;

    (0, _classCallCheck2.default)(this, WipePostFX);
    _this = _super.call(this, {
      game: game,
      name: 'WipePostFX',
      shaders: [{
        name: 'Wipe',
        fragShader: wipeFragShader
      }]
    });
    _this.progress = 0;
    _this.wipeWidth = 0.1;
    _this.direction = 0;
    _this.axis = 0;
    _this.reveal = 0;
    return _this;
  }
  /**
   * @ignore
   */


  (0, _createClass2.default)(WipePostFX, [{
    key: "onBoot",
    value: function onBoot() {
      this.setTexture();
    }
    /**
     * Set the width of the wipe effect.
     * 
     * The value is given as a percentage of the overall texture width, from 0 to 1.
     *
     * @param {number} [width=0.1] - The width of the effect.
     * @returns {this}
     * @memberof WipePostFX
     */

  }, {
    key: "setWipeWidth",
    value: function setWipeWidth() {
      var width = arguments.length > 0 && arguments[0] !== undefined ? arguments[0] : 0.1;
      this.wipeWidth = width;
      return this;
    }
    /**
     * Set the effect to use a Left to Right transition.
     *
     * @returns {this}
     * @memberof WipePostFX
     */

  }, {
    key: "setLeftToRight",
    value: function setLeftToRight() {
      this.direction = 0;
      this.axis = 0;
      return this;
    }
    /**
     * Set the effect to use a Right to Left transition.
     *
     * @returns {this}
     * @memberof WipePostFX
     */

  }, {
    key: "setRightToLeft",
    value: function setRightToLeft() {
      this.direction = 1;
      this.axis = 0;
      return this;
    }
    /**
     * Set the effect to use a Top to Bottom transition.
     *
     * @returns {this}
     * @memberof WipePostFX
     */

  }, {
    key: "setTopToBottom",
    value: function setTopToBottom() {
      this.direction = 1;
      this.axis = 1;
      return this;
    }
    /**
     * Set the effect to use a Bottom to Top transition.
     *
     * @returns {this}
     * @memberof WipePostFX
     */

  }, {
    key: "setBottomToTop",
    value: function setBottomToTop() {
      this.direction = 0;
      this.axis = 1;
      return this;
    }
    /**
     * Use a wipe effect.
     * 
     * A wipe effect will wipe from one texture to another.
     * 
     * The alternative is {@link setRevealEffect}.
     *
     * @returns {this}
     * @memberof WipePostFX
     */

  }, {
    key: "setWipeEffect",
    value: function setWipeEffect() {
      this.reveal = 0;
      return this;
    }
    /**
     * Use a reveal effect.
     * 
     * A reveal effect will wipe from a blank (invisible) texture to the object this pipeline is applied to.
     * 
     * The alternative is {@link setWipeEffect}.
     *
     * @returns {this}
     * @memberof WipePostFX
     */

  }, {
    key: "setRevealEffect",
    value: function setRevealEffect() {
      this.wipeTexture = this.game.textures.getFrame('__DEFAULT').glTexture;
      this.reveal = 1;
      return this;
    }
    /**
     * Set the texture to be wiped-to, or revealed.
     * 
     * The texture must be already loaded and available from the Texture Manager.
     *
     * @param {string} [texture='__DEFAULT'] - The key of the texture to use.
     * @returns {this}
     * @memberof WipePostFX
     */

  }, {
    key: "setTexture",
    value: function setTexture() {
      var texture = arguments.length > 0 && arguments[0] !== undefined ? arguments[0] : '__DEFAULT';
      var phaserTexture = this.game.textures.getFrame(texture);

      if (phaserTexture) {
        this.wipeTexture = phaserTexture.glTexture;
      } else {
        this.wipeTexture = this.game.textures.getFrame('__DEFAULT').glTexture;
      }

      this.set1i('uMainSampler2', 1);
      return this;
    }
    /**
     * Sets the progress of this effect.
     * 
     * Progress is given as a value between 0 and 1.
     *
     * @param {number} [value=0] - The progress of the effect.
     * @returns {this}
     * @memberof WipePostFX
     */

  }, {
    key: "setProgress",
    value: function setProgress() {
      var value = arguments.length > 0 && arguments[0] !== undefined ? arguments[0] : 0;
      this.progress = value;
      return this;
    }
    /**
     * @ignore
     */

  }, {
    key: "onPreRender",
    value: function onPreRender() {
      this.set4f('uInput', this.progress, this.wipeWidth, this.direction, this.axis);
      this.set1f('uReveal', this.reveal);
    }
    /**
     * @ignore
     */

  }, {
    key: "onDraw",
    value: function onDraw(renderTarget) {
      this.set2f('uResolution', renderTarget.width, renderTarget.height);
      this.bindTexture(this.wipeTexture, 1);
      this.bindAndDraw(renderTarget);
    }
  }]);
  return WipePostFX;
}(_phaser.default.Renderer.WebGL.Pipelines.PostFXPipeline);

exports.WipePostFX = WipePostFX;