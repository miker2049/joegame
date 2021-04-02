"use strict";

var _interopRequireDefault = require("@babel/runtime/helpers/interopRequireDefault");

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.MapObject = void 0;

var _classCallCheck2 = _interopRequireDefault(require("@babel/runtime/helpers/classCallCheck"));

var _createClass2 = _interopRequireDefault(require("@babel/runtime/helpers/createClass"));

var _assertThisInitialized2 = _interopRequireDefault(require("@babel/runtime/helpers/assertThisInitialized"));

var _inherits2 = _interopRequireDefault(require("@babel/runtime/helpers/inherits"));

var _possibleConstructorReturn2 = _interopRequireDefault(require("@babel/runtime/helpers/possibleConstructorReturn"));

var _getPrototypeOf2 = _interopRequireDefault(require("@babel/runtime/helpers/getPrototypeOf"));

require("phaser");

function _createForOfIteratorHelper(o, allowArrayLike) { var it; if (typeof Symbol === "undefined" || o[Symbol.iterator] == null) { if (Array.isArray(o) || (it = _unsupportedIterableToArray(o)) || allowArrayLike && o && typeof o.length === "number") { if (it) o = it; var i = 0; var F = function F() {}; return { s: F, n: function n() { if (i >= o.length) return { done: true }; return { done: false, value: o[i++] }; }, e: function e(_e) { throw _e; }, f: F }; } throw new TypeError("Invalid attempt to iterate non-iterable instance.\nIn order to be iterable, non-array objects must have a [Symbol.iterator]() method."); } var normalCompletion = true, didErr = false, err; return { s: function s() { it = o[Symbol.iterator](); }, n: function n() { var step = it.next(); normalCompletion = step.done; return step; }, e: function e(_e2) { didErr = true; err = _e2; }, f: function f() { try { if (!normalCompletion && it.return != null) it.return(); } finally { if (didErr) throw err; } } }; }

function _unsupportedIterableToArray(o, minLen) { if (!o) return; if (typeof o === "string") return _arrayLikeToArray(o, minLen); var n = Object.prototype.toString.call(o).slice(8, -1); if (n === "Object" && o.constructor) n = o.constructor.name; if (n === "Map" || n === "Set") return Array.from(o); if (n === "Arguments" || /^(?:Ui|I)nt(?:8|16|32)(?:Clamped)?Array$/.test(n)) return _arrayLikeToArray(o, minLen); }

function _arrayLikeToArray(arr, len) { if (len == null || len > arr.length) len = arr.length; for (var i = 0, arr2 = new Array(len); i < len; i++) { arr2[i] = arr[i]; } return arr2; }

function _createSuper(Derived) { var hasNativeReflectConstruct = _isNativeReflectConstruct(); return function _createSuperInternal() { var Super = (0, _getPrototypeOf2.default)(Derived), result; if (hasNativeReflectConstruct) { var NewTarget = (0, _getPrototypeOf2.default)(this).constructor; result = Reflect.construct(Super, arguments, NewTarget); } else { result = Super.apply(this, arguments); } return (0, _possibleConstructorReturn2.default)(this, result); }; }

function _isNativeReflectConstruct() { if (typeof Reflect === "undefined" || !Reflect.construct) return false; if (Reflect.construct.sham) return false; if (typeof Proxy === "function") return true; try { Boolean.prototype.valueOf.call(Reflect.construct(Boolean, [], function () {})); return true; } catch (e) { return false; } }

var MapObject = /*#__PURE__*/function (_Phaser$GameObjects$S) {
  (0, _inherits2.default)(MapObject, _Phaser$GameObjects$S);

  var _super = _createSuper(MapObject);

  function MapObject(scene, tilemap, x, y, t_obj) {
    var _this;

    (0, _classCallCheck2.default)(this, MapObject);
    _this = _super.call(this, scene, x, y, ''); // this.setPipeline(joegameData.globalPipeline);

    _this.props = {};
    _this.name = t_obj.name || "".concat(_this.x.toString(), "+").concat(_this.x.toString());
    _this.id = t_obj.id;
    _this.tiledWidth = t_obj.width || 2;
    _this.tiledHeight = t_obj.height || 2;

    if (!t_obj.properties) {// console.log(`SCENEMAP/TILEDOBJECTS: ${this.name} does not have ANY defined properties, btw`)
    } else {
      var _iterator = _createForOfIteratorHelper(t_obj.properties),
          _step;

      try {
        for (_iterator.s(); !(_step = _iterator.n()).done;) {
          var prop = _step.value;

          _this.setData(prop.name, prop.value);
        }
      } catch (err) {
        _iterator.e(err);
      } finally {
        _iterator.f();
      }
    }

    if (t_obj.gid != undefined) {
      if (_this.scene.textures.exists(t_obj.gid.toString())) {
        _this.setTexture(t_obj.gid.toString());
      } else {
        //if there is a gid but not a texture itself, its in one of the tilesheets/spritemaps
        var found = tilemap.tilesets.find(function (tset, ind, tsets) {
          var _tsets;

          // is the gid in question equal to or over this sets first gid? Ok, is it beneath the next one, or already on the last one?
          return t_obj.gid >= tset.firstgid && tsets[ind + 1] ? t_obj.gid < ((_tsets = tsets[ind + 1]) === null || _tsets === void 0 ? void 0 : _tsets.firstgid) : true;
        });

        if (found) {
          _this.setTexture(found.name, t_obj.gid - found.firstgid);
        }
      }
    }

    _this.setFlipX(t_obj.flippedHorizontal || false);

    _this.setFlipY(t_obj.flippedVertical || false);

    _this.setDepth(t_obj.depth);

    _this.setRotation(Phaser.Math.DegToRad(t_obj.rotation || 0));

    _this.setOrigin(0, 1);

    _this.setDisplaySize(_this.tiledWidth, _this.tiledHeight);

    _this.setSize(_this.tiledWidth, _this.tiledHeight);

    if (_this.getData('body') || false) {
      var bodytype = _this.getData('moveable') ? Phaser.Physics.Arcade.DYNAMIC_BODY : Phaser.Physics.Arcade.STATIC_BODY;

      _this.scene.physics.world.enableBody((0, _assertThisInitialized2.default)(_this), bodytype); // scenemap.objbody.add(this)

    }

    if (_this.getData('scrollFactor') || false) {
      var sf = _this.getData('scrollFactor');

      _this.setScrollFactor(sf); // scenemap.objbody.add(this)

    } // this.setSize(this.width,this.height);


    _this.setVisible(t_obj.visible || true); // console.log(`${this.name} is being created!`);


    _this.scene.events.addListener("play_anim_".concat(t_obj.name), function () {
      _this.playAnim();
    });

    _this.scene.events.addListener("stop_anim_".concat(t_obj.name), function () {
      _this.stopAnim();
    });

    _this.scene.events.addListener(_this.getData('animHook') || '', function () {
      _this.playAnim();
    });

    return _this;
  }

  (0, _createClass2.default)(MapObject, [{
    key: "playAnim",
    value: function playAnim() {
      var anim_ = this.getData('anim');

      if (anim_) {
        this.anims.play(anim_);
        this.setDisplaySize(this.width, this.height);
      } else {
        "No anim set on the ${this.name} tiled object (or elsewhere!)";
      }
    }
  }, {
    key: "stopAnim",
    value: function stopAnim() {
      this.anims.stop();
    }
  }]);
  return MapObject;
}(Phaser.GameObjects.Sprite);

exports.MapObject = MapObject;
//# sourceMappingURL=MapObject.js.map