"use strict";

var _interopRequireDefault = require("@babel/runtime/helpers/interopRequireDefault");

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.PageCurlPostFX = void 0;

var _classCallCheck2 = _interopRequireDefault(require("@babel/runtime/helpers/classCallCheck"));

var _createClass2 = _interopRequireDefault(require("@babel/runtime/helpers/createClass"));

var _inherits2 = _interopRequireDefault(require("@babel/runtime/helpers/inherits"));

var _possibleConstructorReturn2 = _interopRequireDefault(require("@babel/runtime/helpers/possibleConstructorReturn"));

var _getPrototypeOf2 = _interopRequireDefault(require("@babel/runtime/helpers/getPrototypeOf"));

var _phaser = _interopRequireDefault(require("phaser"));

function _createSuper(Derived) { var hasNativeReflectConstruct = _isNativeReflectConstruct(); return function _createSuperInternal() { var Super = (0, _getPrototypeOf2.default)(Derived), result; if (hasNativeReflectConstruct) { var NewTarget = (0, _getPrototypeOf2.default)(this).constructor; result = Reflect.construct(Super, arguments, NewTarget); } else { result = Super.apply(this, arguments); } return (0, _possibleConstructorReturn2.default)(this, result); }; }

function _isNativeReflectConstruct() { if (typeof Reflect === "undefined" || !Reflect.construct) return false; if (Reflect.construct.sham) return false; if (typeof Proxy === "function") return true; try { Boolean.prototype.valueOf.call(Reflect.construct(Boolean, [], function () {})); return true; } catch (e) { return false; } }

var fragShader = "\n#define SHADER_NAME PAGE_CURL_FS\n\nprecision mediump float;\n\nuniform sampler2D uMainSampler;\nuniform sampler2D uMainSampler2;\n\nuniform int resizeMode;\nuniform vec2 resolution;\nuniform vec2 from;\nuniform vec2 to;\nuniform float radius;\nuniform float fromRatio;\nuniform float toRatio;\n\nvarying vec2 outFragCoord;\n\n#define pi 3.14159265359\n\nvec4 getFromColor (vec2 uv)\n{\n    return texture2D(uMainSampler, uv);\n}\n\nvec4 getToColor (vec2 uv)\n{\n    if (resizeMode == 2)\n    {\n        //  cover\n        return texture2D(uMainSampler2, 0.5 + (vec2(uv.x, 1.0 - uv.y) - 0.5) * vec2(min(fromRatio / toRatio, 1.0), min((toRatio / fromRatio), 1.0)));\n    }\n    else if (resizeMode == 1)\n    {\n        //  contain\n        return texture2D(uMainSampler2, 0.5 + (vec2(uv.x, 1.0 - uv.y) - 0.5) * vec2(max(fromRatio / toRatio, 1.0), max((toRatio / fromRatio), 1.0)));\n    }\n    else\n    {\n        //  stretch\n        return texture2D(uMainSampler2, vec2(uv.x, 1.0 - uv.y));\n    }\n}\n\nvec4 transition (vec2 p)\n{\n    float aspect = fromRatio;\n\n    vec2 uv = p * vec2(aspect, 1.0) / resolution.xy;\n    vec2 mouse = to * vec2(aspect, 1.0) / resolution.xy;\n    vec2 mouseDir = normalize(abs(from) - to);\n    vec2 origin = clamp(mouse - mouseDir * mouse.x / mouseDir.x, 0.0, 1.0);\n    \n    float mouseDist = clamp(length(mouse - origin) \n        + (aspect - (abs(from.x) / resolution.x) * aspect) / mouseDir.x, 0.0, aspect / mouseDir.x);\n    \n    if (mouseDir.x < 0.0)\n    {\n        mouseDist = distance(mouse, origin);\n    }\n\n    float proj = dot(uv - origin, mouseDir);\n    float dist = proj - mouseDist;\n    \n    vec2 linePoint = uv - dist * mouseDir;\n    vec4 color;\n\n    if (dist > radius) \n    {\n        color = getToColor(uv * vec2(1.0 / aspect, 1.0));\n        color.rgb *= pow(clamp(dist - radius, 0.0, 1.0) * 1.5, 0.2);\n    }\n    else if (dist >= 0.0)\n    {\n        //  Map to cylinder point\n        float theta = asin(dist / radius);\n        vec2 p2 = linePoint + mouseDir * (pi - theta) * radius;\n        vec2 p1 = linePoint + mouseDir * theta * radius;\n        uv = (p2.x <= aspect && p2.y <= 1.0 && p2.x > 0.0 && p2.y > 0.0) ? p2 : p1;\n\n        color = getFromColor(uv * vec2(1.0 / aspect, 1.0));\n        color.rgb *= pow(clamp((radius - dist) / radius, 0.0, 1.0), 0.2);\n    }\n    else \n    {\n        vec2 p = linePoint + mouseDir * (abs(dist) + pi * radius);\n\n        uv = (p.x <= aspect && p.y <= 1.0 && p.x > 0.0 && p.y > 0.0) ? p : uv;\n\n        color = getFromColor(uv * vec2(1.0 / aspect, 1.0));\n    }\n\n    return color;\n}\n    \nvoid main ()\n{\n    gl_FragColor = transition(gl_FragCoord.xy);\n}\n";

var PageCurlPostFX = /*#__PURE__*/function (_Phaser$Renderer$WebG) {
  (0, _inherits2.default)(PageCurlPostFX, _Phaser$Renderer$WebG);

  var _super = _createSuper(PageCurlPostFX);

  /**
   * The WebGL Texture being transitioned to.
   *
   * @type {WebGLTexture}
   * @memberof PageCurlPostFX
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
   * @memberof PageCurlPostFX
   */

  /**
   * The ratio of the target texture (width / height).
   * 
   * This is set automatically in the `setTexture` method.
   *
   * @type {number}
   * @memberof PageCurlPostFX
   */

  /**
   * The radius of the curl.
   * 
   * Use very low values. The default is 0.1.
   * 
   * @type {number}
   * @memberof PageCurlPostFX
   */

  /**
   * The position the effect runs from.
   * 
   * See also the `setFrom` method.
   * 
   * @type {Phaser.Math.Vector2}
   * @memberof PageCurlPostFX
   */

  /**
   * The position the effect runs to.
   * 
   * See also the `setTo` method.
   * 
   * @type {Phaser.Math.Vector2}
   * @memberof PageCurlPostFX
   */

  /**
   * The Page Curl FX allows you to create a classic 'page curl' motion
   * between two sources. The curl proceeds from either the top-left or
   * top-right of the source. You can control the position vectors
   * independantly, as well as the radius of the curl.
   * 
   * The source image comes from the Game Object to which the FX is applied,
   * which can be any Game Object that supports post pipelines, such as a
   * Sprite, Rope or Layer. You can also transition Cameras and even entire
   * Scenes. Please see the examples and class docs for further details.
   * 
   * @param {Phaser.Game} game
   * @memberof PageCurlPostFX
   */
  function PageCurlPostFX(game) {
    var _this;

    (0, _classCallCheck2.default)(this, PageCurlPostFX);
    _this = _super.call(this, {
      game: game,
      name: 'PageCurlPostFX',
      fragShader: fragShader
    });
    _this.resizeMode = 0;
    _this.toRatio = 0;
    _this.radius = 0.1;
    _this.from = new _phaser.default.Math.Vector2();
    _this.to = new _phaser.default.Math.Vector2();
    return _this;
  }
  /**
   * @ignore
   */


  (0, _createClass2.default)(PageCurlPostFX, [{
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
     * @memberof PageCurlPostFX
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
     * @memberof PageCurlPostFX
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
     * Sets the position the effect should start from.
     * 
     * The values are given in pixels and should typically be restricted
     * to the range of the texture size the effect is running on.
     * 
     * @param {number} x - The x position.
     * @param {number} y - The y position.
     * @returns {this}
     * @memberof PageCurlPostFX
     */

  }, {
    key: "setFrom",
    value: function setFrom(x, y) {
      this.from.set(x, y);
      return this;
    }
    /**
     * Sets the position the effect should run to.
     * 
     * The values are given in pixels and should typically be restricted
     * to the range of the texture size the effect is running on.
     * 
     * @param {number} x - The x position.
     * @param {number} y - The y position.
     * @returns {this}
     * @memberof PageCurlPostFX
     */

  }, {
    key: "setTo",
    value: function setTo(x, y) {
      this.to.set(x, y);
      return this;
    }
    /**
     * Sets the radius of the curl effect.
     * 
     * Should be a small value. The defaul is 0.1.
     * 
     * @param {number} [value=0.1] - The radius.
     * @returns {this}
     * @memberof PageCurlPostFX
    */

  }, {
    key: "setRadius",
    value: function setRadius() {
      var value = arguments.length > 0 && arguments[0] !== undefined ? arguments[0] : 0.1;
      this.radius = value;
      return this;
    }
    /**
     * @ignore
     */

  }, {
    key: "onPreRender",
    value: function onPreRender() {
      this.set1i('resizeMode', this.resizeMode);
      this.set1f('radius', this.radius);
    }
    /**
     * @ignore
     */

  }, {
    key: "onDraw",
    value: function onDraw(renderTarget) {
      var w = renderTarget.width;
      var h = renderTarget.height;
      this.set1f('fromRatio', w / h);
      this.set2f('resolution', w, h);
      this.set2f('from', this.from.x, h - this.from.y);
      this.set2f('to', this.to.x, h - this.to.y);
      this.bindTexture(this.targetTexture, 1);
      this.bindAndDraw(renderTarget);
    }
  }]);
  return PageCurlPostFX;
}(_phaser.default.Renderer.WebGL.Pipelines.PostFXPipeline);

exports.PageCurlPostFX = PageCurlPostFX;