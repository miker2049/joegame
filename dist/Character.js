"use strict";

var _interopRequireDefault = require("@babel/runtime/helpers/interopRequireDefault");

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.default = void 0;

var _regenerator = _interopRequireDefault(require("@babel/runtime/regenerator"));

var _asyncToGenerator2 = _interopRequireDefault(require("@babel/runtime/helpers/asyncToGenerator"));

var _classCallCheck2 = _interopRequireDefault(require("@babel/runtime/helpers/classCallCheck"));

var _createClass2 = _interopRequireDefault(require("@babel/runtime/helpers/createClass"));

var _assertThisInitialized2 = _interopRequireDefault(require("@babel/runtime/helpers/assertThisInitialized"));

var _inherits2 = _interopRequireDefault(require("@babel/runtime/helpers/inherits"));

var _possibleConstructorReturn2 = _interopRequireDefault(require("@babel/runtime/helpers/possibleConstructorReturn"));

var _getPrototypeOf2 = _interopRequireDefault(require("@babel/runtime/helpers/getPrototypeOf"));

require("phaser");

var _charJumpUp = _interopRequireDefault(require("./actions/charJumpUp"));

var _joegameTypes = require("./joegameTypes");

var _getDashVelForDistance = _interopRequireDefault(require("./utils/getDashVelForDistance"));

var _defaults = _interopRequireDefault(require("./defaults"));

var _createResidualGraphic = _interopRequireDefault(require("./actions/createResidualGraphic"));

var _VoxBox = _interopRequireDefault(require("./components/VoxBox"));

function _createSuper(Derived) { var hasNativeReflectConstruct = _isNativeReflectConstruct(); return function _createSuperInternal() { var Super = (0, _getPrototypeOf2.default)(Derived), result; if (hasNativeReflectConstruct) { var NewTarget = (0, _getPrototypeOf2.default)(this).constructor; result = Reflect.construct(Super, arguments, NewTarget); } else { result = Super.apply(this, arguments); } return (0, _possibleConstructorReturn2.default)(this, result); }; }

function _isNativeReflectConstruct() { if (typeof Reflect === "undefined" || !Reflect.construct) return false; if (Reflect.construct.sham) return false; if (typeof Proxy === "function") return true; try { Boolean.prototype.valueOf.call(Reflect.construct(Boolean, [], function () {})); return true; } catch (e) { return false; } }

var Character = /*#__PURE__*/function (_Phaser$GameObjects$C) {
  (0, _inherits2.default)(Character, _Phaser$GameObjects$C);

  var _super = _createSuper(Character);

  //settings
  //private seetings
  //state
  function Character(config) {
    var _config$body, _config$body2, _config$body3, _config$body4;

    var _this;

    (0, _classCallCheck2.default)(this, Character);
    _this = _super.call(this, config.level.scene, config.x, config.y);
    _this.level = config.level; //assign settings from config

    _this.speed = config.speed;
    _this.name = config.name;
    _this.dashDistance = config.dashDistance;
    _this.charge = 2;
    _this.animKeys = config.anims;
    _this.dashDrag = 300;
    _this.dashVel = (0, _getDashVelForDistance.default)(_this.dashDistance, _this.dashDrag);
    _this.sprite = config.level.scene.make.sprite({
      key: config.texture
    }, false);

    _this.sprite.removeFromDisplayList();

    _this.voxbox = new _VoxBox.default(_this.level);

    _this.voxbox.close();

    _this.setDepth(_this.depth = config.depth || _defaults.default.charDepth);

    _this.face(_joegameTypes.Dir.south);

    _this.facing = _joegameTypes.Dir.south;
    _this.onPlatform = false;
    _this.auto = false;
    _this.player = false;
    _this.groundVel = {
      x: 0,
      y: 0
    }; // this.sprite.setTintFill(Phaser.Display.Color.RandomRGB().color)
    // this.setSize(this.scene.tileWidth/2,this.scene.tileHeight/2)

    _this.sprite.setScale(config.scale);

    _this.setInteractive(new Phaser.Geom.Circle(0, 0, _this.level.map.tileWidth * 2), Phaser.Geom.Circle.Contains);

    _this.level.scene.physics.world.enable((0, _assertThisInitialized2.default)(_this), Phaser.Physics.Arcade.DYNAMIC_BODY);

    _this.charBody = _this.body;

    _this.add(_this.sprite);

    _this.charBody.setSize(((_config$body = config.body) === null || _config$body === void 0 ? void 0 : _config$body.width) || _this.level.map.tileWidth * 0.5, ((_config$body2 = config.body) === null || _config$body2 === void 0 ? void 0 : _config$body2.height) || _this.level.map.tileHeight * 0.5);

    _this.sprite.setPosition(_this.charBody.width / 2, 0);

    _this.charBody.setOffset(((_config$body3 = config.body) === null || _config$body3 === void 0 ? void 0 : _config$body3.offsetX) || 0, ((_config$body4 = config.body) === null || _config$body4 === void 0 ? void 0 : _config$body4.offsetY) || 0);

    _this.add(_this.voxbox); // this.sprite.on('animationstart', (anim, frame) => { console.log(`start of ${anim}`) })
    // this.sprite.on('animationrepeat', (anim, frame) => { console.log(`update of ${anim}`) })
    // this.sprite.on('animationupdate', (anim, frame) => {
    //     console.log(`is ${anim}, frame is ${frame}`)
    // })
    // let pnt=this.scene.make.graphics({x:0,y:0}).fillStyle(Phaser.Display.Color.GetColor(255,0,0)).fillCircle(this.charBody.width/2,this.charBody.height/2,1).setDepth(10)
    // let pnt2=this.scene.make.graphics({x:0,y:0}).fillStyle(Phaser.Display.Color.GetColor(0,255,0)).fillCircle(this.charBody.top,0,1).setDepth(10)
    // let pnt=this.scene.make.graphics({x:0,y:0}).fillStyle(Phaser.Display.Color.GetColor(255,0,0)).fillCircle(0,0,1).setDepth(10)
    // this.add([pnt2])


    return _this;
  } //control


  (0, _createClass2.default)(Character, [{
    key: "changeGroundVel",
    value: function changeGroundVel() {
      var _this2 = this;

      var platform;
      this.scene.physics.world.overlap(this, this.level.platforms, function (player, plat) {
        platform = plat;

        if (platform != undefined) {
          _this2.groundVel = {
            x: platform.body.velocity.x,
            y: platform.body.velocity.y
          };
        } else {
          _this2.groundVel = {
            x: 0,
            y: 0
          };
        }
      });
    }
  }, {
    key: "move",
    value: function move(dir) {
      this.charBody.allowDrag = false;
      this.facing = dir;
      this.playAnim(this.animKeys[_joegameTypes.Dir[dir]]);
      this.charBody.setVelocity(this.groundVel.x || 0, this.groundVel.y || 0); // this is needed again to cancel diagonole shit

      this.charBody.velocity.add(new Phaser.Math.Vector2(this.speed * _joegameTypes.VelocityMap[dir][0], this.speed * _joegameTypes.VelocityMap[dir][1]));
    }
  }, {
    key: "dash",
    value: function dash(dir) {
      this.showDashboxes();
      this.charBody.allowDrag = true;
      this.charBody.setDrag(this.dashDrag, this.dashDrag);
      this.charBody.setVelocity(0);
      this.charBody.setAcceleration(0, 0);
      this.face(dir);
      this.stopAnim();
      this.charBody.setVelocity(this.dashVel * _joegameTypes.VelocityMap[dir][0] + (this.groundVel.x || 0), this.dashVel * _joegameTypes.VelocityMap[dir][1] + (this.groundVel.y || 0));
      (0, _createResidualGraphic.default)(this.sprite, this.x, this.y);
    }
  }, {
    key: "face",
    value: function face(dir) {
      var dirAnim = this.scene.anims.get(this.animKeys[dir]); //NOTE this if should be unecessary

      if (dirAnim) {
        this.sprite.anims.setCurrentFrame(dirAnim.frames[1]);
        this.facing = dir;
      }
    }
  }, {
    key: "stop",
    value: function stop(face) {
      this.charBody.allowDrag = false;
      this.charBody.setVelocity(this.groundVel.x || 0, this.groundVel.y || 0);
      this.stopAnim();

      if (face) {
        this.face(face);
        this.facing = face;
      }
    }
  }, {
    key: "align",
    value: function align() {
      var x_ = Math.floor(this.charBody.center.x / this.level.map.tileWidth) * this.level.map.tileWidth;
      var y_ = Math.floor(this.charBody.center.y / this.level.map.tileHeight) * this.level.map.tileHeight;
      this.transport(x_ + this.level.map.tileWidth / 2, y_ + this.level.map.tileHeight / 2); // this.transport(x_, y_)

      return {
        x: x_ / this.level.map.tileWidth,
        y: y_ / this.level.map.tileHeight
      };
    }
  }, {
    key: "jumpUp",
    value: function jumpUp() {
      (0, _charJumpUp.default)(this);
    }
    /*
     * The dir here marks the direction where you are jumping back from
     */

  }, {
    key: "jumpBack",
    value: function jumpBack(dir) {
      console.log(this.name + " is jumping!");
      this.jumpUp();

      switch (dir) {
        //if collider.body.touching.up
        case _joegameTypes.Dir.north:
          {
            this.face(_joegameTypes.Dir.north);
            this.transportNudge(0, 8);
            this.align();
            break;
          }

        case _joegameTypes.Dir.south:
          {
            this.face(_joegameTypes.Dir.south);
            this.transportNudge(0, -8);
            this.align();
            break;
          }

        case _joegameTypes.Dir.east:
          {
            this.face(_joegameTypes.Dir.east);
            this.transportNudge(-8, 0);
            this.align();
            break;
          }

        case _joegameTypes.Dir.west:
          {
            this.face(_joegameTypes.Dir.west);
            this.transportNudge(8, 0);
            this.align();
            break;
          }
      }
    }
  }, {
    key: "transport",
    value: function transport(x, y) {
      this.setPosition(x + (this.x - this.charBody.center.x), y + (this.y - this.charBody.center.y));
    }
  }, {
    key: "transportNudge",
    value: function transportNudge(x, y) {
      this.setPosition(this.x + x, this.y + y);
    }
  }, {
    key: "minusCharge",
    value: function minusCharge() {}
  }, {
    key: "playAnim",
    value: function playAnim(anim) {
      this.sprite.anims.play({
        key: anim,
        delay: 0,
        repeat: -1,
        frameRate: 11,
        startFrame: 1
      }, false);
    }
  }, {
    key: "stopAnim",
    value: function stopAnim() {
      this.sprite.anims.stop();
      var dirAnim = this.scene.anims.get(this.animKeys[this.facing]);
      this.sprite.anims.setCurrentFrame(dirAnim.frames[1]);
    } // UI control

  }, {
    key: "showDashboxes",
    value: function showDashboxes() {}
  }, {
    key: "minusDashbox",
    value: function minusDashbox() {}
  }, {
    key: "showLabel",
    value: function showLabel() {}
  }, {
    key: "hideLabel",
    value: function hideLabel() {}
  }, {
    key: "speak",
    value: function () {
      var _speak = (0, _asyncToGenerator2.default)( /*#__PURE__*/_regenerator.default.mark(function _callee(msg, speed) {
        return _regenerator.default.wrap(function _callee$(_context) {
          while (1) {
            switch (_context.prev = _context.next) {
              case 0:
                _context.next = 2;
                return this.voxbox.speak(msg, speed !== null && speed !== void 0 ? speed : 25);

              case 2:
              case "end":
                return _context.stop();
            }
          }
        }, _callee, this);
      }));

      function speak(_x, _x2) {
        return _speak.apply(this, arguments);
      }

      return speak;
    }() //move machine
    // sendMsgToMoveMachine(msg: string): void { }
    // startMoveMachine(): void { }
    // pauseMoveMachine(): void { }
    // killMoveMachine(): void { }

  }]);
  return Character;
}(Phaser.GameObjects.Container);

exports.default = Character;
//# sourceMappingURL=Character.js.map