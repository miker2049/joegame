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

var _closestMultiple = _interopRequireDefault(require("../utils/closestMultiple"));

var _defaults = _interopRequireDefault(require("../defaults"));

function _createSuper(Derived) { var hasNativeReflectConstruct = _isNativeReflectConstruct(); return function _createSuperInternal() { var Super = (0, _getPrototypeOf2.default)(Derived), result; if (hasNativeReflectConstruct) { var NewTarget = (0, _getPrototypeOf2.default)(this).constructor; result = Reflect.construct(Super, arguments, NewTarget); } else { result = Super.apply(this, arguments); } return (0, _possibleConstructorReturn2.default)(this, result); }; }

function _isNativeReflectConstruct() { if (typeof Reflect === "undefined" || !Reflect.construct) return false; if (Reflect.construct.sham) return false; if (typeof Proxy === "function") return true; try { Boolean.prototype.valueOf.call(Reflect.construct(Boolean, [], function () {})); return true; } catch (e) { return false; } }

var Platform = /*#__PURE__*/function (_Phaser$GameObjects$C) {
  (0, _inherits2.default)(Platform, _Phaser$GameObjects$C);

  var _super = _createSuper(Platform);

  function Platform(config) {
    var _this;

    (0, _classCallCheck2.default)(this, Platform);
    _this = _super.call(this, config.level.scene, config.x * config.level.map.tileWidth, config.y * config.level.map.tileHeight);
    (0, _defineProperty2.default)((0, _assertThisInitialized2.default)(_this), "speed", 5);
    (0, _defineProperty2.default)((0, _assertThisInitialized2.default)(_this), "acceleration", 100);
    (0, _defineProperty2.default)((0, _assertThisInitialized2.default)(_this), "pause", 2000);
    (0, _defineProperty2.default)((0, _assertThisInitialized2.default)(_this), "velX", 0);
    (0, _defineProperty2.default)((0, _assertThisInitialized2.default)(_this), "velY", 0);
    (0, _defineProperty2.default)((0, _assertThisInitialized2.default)(_this), "atHome", true);
    (0, _defineProperty2.default)((0, _assertThisInitialized2.default)(_this), "tileSize", 16);
    _this.level = config.level;

    _this.setName(config.name || "".concat(config.x, "x").concat(config.y, "x").concat(config.endX, "x").concat(config.endY)); //TODO make this dynamic


    var ptype = _this.scene.cache.json.get('gdata').platform.get(config.ptype || 'default');

    _this.tileSize = config.level.map.tileWidth;
    var tiles = [];

    for (var i = 0; i < config.width; i++) {
      for (var j = 0; j < config.height; j++) {
        var groundVersion = ptype.groundTiles[Math.floor(Math.random() * ptype.groundTiles.length)];
        tiles.push(_this.scene.add.image(i * config.level.map.tileWidth, j * config.level.map.tileHeight, ptype.texture, groundVersion).setOrigin(0, 0).setDepth(_defaults.default.platformDepth));

        if (j === config.height - 1) {
          tiles.push(_this.scene.add.image(i * config.level.map.tileWidth, j * config.level.map.tileHeight + config.level.map.tileHeight, ptype.texture, ptype.edgeTiles[0]).setOrigin(0, 0).setDepth(_defaults.default.platformDepth - 1));
        }
      }
    }

    _this.setSize(config.width * config.level.map.tileWidth, config.height * config.level.map.tileHeight);

    _this.add(tiles);

    console.log(tiles); // this.body = new Phaser.Physics.Arcade.Body(this.scene.physics.world,this)

    _this.scene.physics.world.enable((0, _assertThisInitialized2.default)(_this));

    _this.body.setOffset(config.width * config.level.map.tileWidth / 2, config.height * config.level.map.tileHeight / 2);

    _this.runPlatform(config);

    return _this;
  }

  (0, _createClass2.default)(Platform, [{
    key: "runPlatform",
    value: function runPlatform(config) {
      var _this2 = this;

      var distance = Phaser.Math.Distance.Between(config.x, config.y, config.endX || config.x, config.endY || config.y) * this.tileSize;
      var delay = (config.speed || 1) * 1000;
      var speed = Math.floor(distance / (delay / 1000));
      this.velX = Math.abs((config.endX || config.x) - config.x) > 1 ? (config.endX || config.x) > config.x ? speed : -speed : 0;
      this.velY = Math.abs((config.endY || config.y) - config.y) > 1 ? (config.endY || config.y) > config.y ? speed : -speed : 0;
      this.velX = this.velX * -1;
      this.velY = this.velY * -1;
      this.atHome = false;
      this.body.setMaxSpeed(speed);

      var cb = function cb() {
        var newVelX = _this2.velX;
        var newVelY = _this2.velY; // const accelX = this.velX != 0 ? this.velX > 0 ?  defaults.globalDrag : -defaults.globalDrag : 0
        // const accelY = this.velY != 0 ? this.velY > 0 ?  defaults.globalDrag : -defaults.globalDrag : 0
        // this.body.setAcceleration(accelX,accelY)

        _this2.body.setVelocity(newVelX, newVelY);

        _this2.notifyVelChange();
      };

      this.scene.time.addEvent({
        callback: function callback() {
          _this2.atHome = !!!_this2.atHome;
          var snapX = _this2.atHome ? config.x : config.endX || config.x;
          var snapY = _this2.atHome ? config.y : config.endY || config.y;

          _this2.body.setAcceleration(0, 0);

          _this2.body.setVelocity(0, 0);

          _this2.velX = _this2.velX * -1;
          _this2.velY = _this2.velY * -1;
          _this2.body.x = _this2.x = (0, _closestMultiple.default)(snapX * _this2.tileSize, _this2.tileSize);
          _this2.body.y = _this2.y = (0, _closestMultiple.default)(snapY * _this2.tileSize, _this2.tileSize);

          _this2.notifyVelChange();

          _this2.scene.time.addEvent({
            callback: cb,
            delay: _this2.pause
          });
        },
        delay: delay + this.pause,
        loop: true
      });
    }
  }, {
    key: "notifyVelChange",
    value: function notifyVelChange() {
      var _this3 = this;

      //todo, we need a all player group or something, for when we want npcs on platforms
      // let test= this.scene.physics.overlap(this, this.scene.player, (plat, player)=>{
      //     const char = player as Character;
      //     char.moveMachine.send('PLATFORM_CHANGE', {vel: {x: plat.body.velocity.x, y: plat.body.velocity.y}})
      // })
      // this.scene.physics.overlap(this, this.scene.map.objbody,(plat,obj)=>{
      //     obj.moveMachine.send('PLATFORM_CHANGE', {vel: {x: plat.body.velocity.x, y: plat.body.velocity.y}})
      this.level.scene.physics.overlap(this.level.platforms, this.level.player, function (player, plat) {
        console.log('HEY');

        _this3.level.machineRegisty.sendTo("player_machine", {
          type: 'PLATFORM_CHANGE',
          vel: {
            x: plat.body.velocity.x,
            y: plat.body.velocity.y
          }
        });
      });
    }
  }]);
  return Platform;
}(Phaser.GameObjects.Container);

exports.default = Platform;