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

require("./SceneMap");

var _MapObject2 = _interopRequireDefault(require("./MapObject"));

var _joegameData = require("./joegameData");

function _createSuper(Derived) { var hasNativeReflectConstruct = _isNativeReflectConstruct(); return function _createSuperInternal() { var Super = (0, _getPrototypeOf2.default)(Derived), result; if (hasNativeReflectConstruct) { var NewTarget = (0, _getPrototypeOf2.default)(this).constructor; result = Reflect.construct(Super, arguments, NewTarget); } else { result = Super.apply(this, arguments); } return (0, _possibleConstructorReturn2.default)(this, result); }; }

function _isNativeReflectConstruct() { if (typeof Reflect === "undefined" || !Reflect.construct) return false; if (Reflect.construct.sham) return false; if (typeof Proxy === "function") return true; try { Boolean.prototype.valueOf.call(Reflect.construct(Boolean, [], function () {})); return true; } catch (e) { return false; } }

var Door = /*#__PURE__*/function (_MapObject) {
  (0, _inherits2.default)(Door, _MapObject);

  var _super = _createSuper(Door);

  function Door(scenemap, x, y, t_obj) {
    var _this;

    (0, _classCallCheck2.default)(this, Door);
    _this = _super.call(this, scenemap, x, y, t_obj);

    _this.setTexture(_joegameData.joegameData.DoorTexture);

    _this.setDisplaySize(_this.tiledWidth, _this.tiledHeight);

    _this.scene.events.addListener("open_".concat(t_obj.name), function () {
      _this.openDoor();
    });

    _this.scene.physics.add.existing((0, _assertThisInitialized2.default)(_this), true);

    return _this;
  }

  (0, _createClass2.default)(Door, [{
    key: "collideWith",
    value: function collideWith(obj) {
      this.scene.physics.add.collider(this, obj, function () {
        console.log("hieeeee");
      }, undefined, this.scene);
    }
  }, {
    key: "openDoor",
    value: function openDoor() {
      var _this2 = this;

      if (this.tiledWidth < this.tiledHeight) {
        //then its a door opening vertically
        this.scene.tweens.add({
          targets: this,
          scaleY: {
            from: this.scaleY,
            to: 0
          },
          duration: 2010,
          repeat: 0,
          onComplete: function onComplete() {
            _this2.destroy(true);
          }
        });
        this.scene.tweens.add({
          targets: this.body,
          height: {
            from: this.body.height,
            to: 1
          },
          duration: 2000,
          repeat: 0
        });
      } else {
        this.scene.tweens.add({
          targets: this,
          scaleX: {
            from: this.scaleX,
            to: 0
          },
          duration: 2010,
          repeat: 0,
          onComplete: function onComplete() {
            _this2.destroy(true);
          }
        });
        this.scene.tweens.add({
          targets: this.body,
          width: {
            from: this.body.width,
            to: 1
          },
          duration: 2000,
          repeat: 0
        });
      }
    }
  }]);
  return Door;
}(_MapObject2.default);

exports.default = Door;