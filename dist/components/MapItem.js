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

var _OverlapArea2 = _interopRequireDefault(require("./OverlapArea"));

var _joegameData = require("./joegameData");

function _createSuper(Derived) { var hasNativeReflectConstruct = _isNativeReflectConstruct(); return function _createSuperInternal() { var Super = (0, _getPrototypeOf2.default)(Derived), result; if (hasNativeReflectConstruct) { var NewTarget = (0, _getPrototypeOf2.default)(this).constructor; result = Reflect.construct(Super, arguments, NewTarget); } else { result = Super.apply(this, arguments); } return (0, _possibleConstructorReturn2.default)(this, result); }; }

function _isNativeReflectConstruct() { if (typeof Reflect === "undefined" || !Reflect.construct) return false; if (Reflect.construct.sham) return false; if (typeof Proxy === "function") return true; try { Boolean.prototype.valueOf.call(Reflect.construct(Boolean, [], function () {})); return true; } catch (e) { return false; } }

/**
 * MapItems are MapObjects the player picks up and is added to inventory
 */
var MapItem = /*#__PURE__*/function (_OverlapArea) {
  (0, _inherits2.default)(MapItem, _OverlapArea);

  var _super = _createSuper(MapItem);

  function MapItem(scenemap, x, y, t_obj) {
    var _this;

    (0, _classCallCheck2.default)(this, MapItem);
    _this = _super.call(this, scenemap, x, y, t_obj);

    _this.setOrigin(0, 1);

    var itemData = _joegameData.joegameData.items[_this.name];

    _this.setTexture(itemData.texture, itemData.frame || 0).setScale(itemData.scale || 1);

    _this.callback = function () {
      _this.scene.registry.get('Inventory').push(_this.name);

      _this.scene.notify("you picked up ".concat(_this.name));

      _this.sparkles.destroy();

      _this.destroy();
    };

    _this.activateOverlap(_this.scene.player);

    _this.sparkles = _this.scene.add.particles(itemData.particleTexture || 'yellow-particle');

    if (itemData.sparkly) {
      _this.activateSparkles();
    }

    _this.scene.tweens.add({
      targets: [(0, _assertThisInitialized2.default)(_this)],
      y: _this.y - 1,
      ease: 'Sine',
      loop: -1,
      duration: 2000,
      yoyo: true
    });

    return _this;
  }

  (0, _createClass2.default)(MapItem, [{
    key: "activateSparkles",
    value: function activateSparkles() {
      this.sparkles.createEmitter({}).setPosition(this.x + this.width / 2, this.y - this.height / 2).setBlendMode(Phaser.BlendModes.ADD).setSpeed(10).setScale(0.5).setLifespan(500);
    }
  }]);
  return MapItem;
}(_OverlapArea2.default);

exports.default = MapItem;
//# sourceMappingURL=MapItem.js.map