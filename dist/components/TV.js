"use strict";

var _interopRequireDefault = require("@babel/runtime/helpers/interopRequireDefault");

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.default = void 0;

var _classCallCheck2 = _interopRequireDefault(require("@babel/runtime/helpers/classCallCheck"));

var _createClass2 = _interopRequireDefault(require("@babel/runtime/helpers/createClass"));

var _assertThisInitialized2 = _interopRequireDefault(require("@babel/runtime/helpers/assertThisInitialized"));

var _inherits2 = _interopRequireDefault(require("@babel/runtime/helpers/inherits"));

var _possibleConstructorReturn2 = _interopRequireDefault(require("@babel/runtime/helpers/possibleConstructorReturn"));

var _getPrototypeOf2 = _interopRequireDefault(require("@babel/runtime/helpers/getPrototypeOf"));

require("phaser");

var _joegameData = require("./joegameData");

function _createSuper(Derived) { var hasNativeReflectConstruct = _isNativeReflectConstruct(); return function _createSuperInternal() { var Super = (0, _getPrototypeOf2.default)(Derived), result; if (hasNativeReflectConstruct) { var NewTarget = (0, _getPrototypeOf2.default)(this).constructor; result = Reflect.construct(Super, arguments, NewTarget); } else { result = Super.apply(this, arguments); } return (0, _possibleConstructorReturn2.default)(this, result); }; }

function _isNativeReflectConstruct() { if (typeof Reflect === "undefined" || !Reflect.construct) return false; if (Reflect.construct.sham) return false; if (typeof Proxy === "function") return true; try { Boolean.prototype.valueOf.call(Reflect.construct(Boolean, [], function () {})); return true; } catch (e) { return false; } }

var TV = /*#__PURE__*/function (_Phaser$GameObjects$S) {
  (0, _inherits2.default)(TV, _Phaser$GameObjects$S);

  var _super = _createSuper(TV);

  function TV(scene, x, y) {
    var _this;

    (0, _classCallCheck2.default)(this, TV);
    _this = _super.call(this, scene, x, y, _joegameData.joegameData.textures.tv);
    _this.video = _this.initVideo('home-movie');

    _this.video.setPipeline(_joegameData.joegameData.globalPipeline);

    _this.setPipeline(_joegameData.joegameData.globalPipeline);

    _this.scene.physics.world.enableBody((0, _assertThisInitialized2.default)(_this), Phaser.Physics.Arcade.STATIC_BODY);

    _this.body.setSize(_this.width - 8, _this.height / 4).setOffset(3, 16);

    _this.registerColliders();

    _this.setDepth(700);

    _this.video.setDepth(600);

    _this.scene.events.on('create', function () {
      _this.initColliders();
    });

    return _this;
  }

  (0, _createClass2.default)(TV, [{
    key: "initColliders",
    value: function initColliders() {
      this.scene.physics.world.addCollider(this, this.scene.npcGroup);
      this.scene.physics.world.addCollider(this, this.scene.player);
    }
  }, {
    key: "initVideo",
    value: function initVideo(vid) {
      var _this$video;

      if ((_this$video = this.video) !== null && _this$video !== void 0 && _this$video.active) {
        this.video.destroy();
      }

      var video = this.scene.add.video(this.x, this.y - 10, vid);
      video.play(true, 0, 3000).setDisplaySize(this.width - 10, this.height / 2);
      return video;
    }
  }, {
    key: "registerColliders",
    value: function registerColliders() {
      var _this2 = this;

      var tiles = this.scene.map.tilemap.getTilesWithinWorldXY(this.body.x, this.body.y, this.body.width, this.body.height, {}, this.scene.cameras.main, this.scene.map.mainLayer); // this.scene.map.highlightTile(11,11)

      tiles.forEach(function (tile) {
        _this2.scene.map.pathfinder.finder.avoidAdditionalPoint(tile.x, tile.y);
      });
      console.log("TVS TILES", tiles);
    }
  }]);
  return TV;
}(Phaser.GameObjects.Sprite);

exports.default = TV;
//# sourceMappingURL=TV.js.map