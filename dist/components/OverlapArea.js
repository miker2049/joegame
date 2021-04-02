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

var _defineProperty2 = _interopRequireDefault(require("@babel/runtime/helpers/defineProperty"));

require("phaser");

var _MapObject2 = _interopRequireDefault(require("./MapObject"));

function _createSuper(Derived) { var hasNativeReflectConstruct = _isNativeReflectConstruct(); return function _createSuperInternal() { var Super = (0, _getPrototypeOf2.default)(Derived), result; if (hasNativeReflectConstruct) { var NewTarget = (0, _getPrototypeOf2.default)(this).constructor; result = Reflect.construct(Super, arguments, NewTarget); } else { result = Super.apply(this, arguments); } return (0, _possibleConstructorReturn2.default)(this, result); }; }

function _isNativeReflectConstruct() { if (typeof Reflect === "undefined" || !Reflect.construct) return false; if (Reflect.construct.sham) return false; if (typeof Proxy === "function") return true; try { Boolean.prototype.valueOf.call(Reflect.construct(Boolean, [], function () {})); return true; } catch (e) { return false; } }

var OverlapArea = /*#__PURE__*/function (_MapObject) {
  (0, _inherits2.default)(OverlapArea, _MapObject);

  var _super = _createSuper(OverlapArea);

  function OverlapArea(scenemap, x, y, t_obj) {
    var _this;

    (0, _classCallCheck2.default)(this, OverlapArea);
    _this = _super.call(this, scenemap, x, y, t_obj);
    (0, _defineProperty2.default)((0, _assertThisInitialized2.default)(_this), "callback", function () {});

    var cb_ = _this.getData('cb');

    if (cb_) {
      _this.callback = function () {
        _this.scene.events.emit(cb_);
      };
    } else {// console.log("No callback defined for overlap area ${this.name}");
    }

    _this.scene.events.addListener("activate_".concat(t_obj.name), function () {
      _this.activateOverlap(_this.scene.player);
    });

    if (_this.getData("active")) {
      _this.scene.events.on('create', function () {
        _this.activateOverlap(_this.scene.player);
      });
    }

    return _this;
  }

  (0, _createClass2.default)(OverlapArea, [{
    key: "activateOverlap",
    value: function activateOverlap(player) {
      var _this2 = this;

      var overlap_id = this.name;
      this.playAnim();
      this.scene.physics.world.enableBody(this);
      this.setDisplaySize(this.tiledWidth, this.tiledHeight);
      this.setSize(this.tiledWidth, this.tiledHeight);
      this.scene.physics.add.overlap(this, player, function () {
        _this2.scene.physics.world.colliders.getActive().find(function (i) {
          return i.name == overlap_id;
        }).destroy();

        _this2.setVisible(false);

        _this2.callback();

        _this2.destroy();
      }, function () {}, this.scene).name = overlap_id;
    }
  }]);
  return OverlapArea;
}(_MapObject2.default);

exports.default = OverlapArea;
//# sourceMappingURL=OverlapArea.js.map