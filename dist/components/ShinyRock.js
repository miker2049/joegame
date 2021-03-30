"use strict";

var _interopRequireDefault = require("@babel/runtime/helpers/interopRequireDefault");

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.default = void 0;

var _classCallCheck2 = _interopRequireDefault(require("@babel/runtime/helpers/classCallCheck"));

var _inherits2 = _interopRequireDefault(require("@babel/runtime/helpers/inherits"));

var _possibleConstructorReturn2 = _interopRequireDefault(require("@babel/runtime/helpers/possibleConstructorReturn"));

var _getPrototypeOf2 = _interopRequireDefault(require("@babel/runtime/helpers/getPrototypeOf"));

require("phaser");

var _MapObject2 = _interopRequireDefault(require("./MapObject"));

var _ProgressBar = _interopRequireDefault(require("./ProgressBar"));

var _xstate = require("xstate");

function _createSuper(Derived) { var hasNativeReflectConstruct = _isNativeReflectConstruct(); return function _createSuperInternal() { var Super = (0, _getPrototypeOf2.default)(Derived), result; if (hasNativeReflectConstruct) { var NewTarget = (0, _getPrototypeOf2.default)(this).constructor; result = Reflect.construct(Super, arguments, NewTarget); } else { result = Super.apply(this, arguments); } return (0, _possibleConstructorReturn2.default)(this, result); }; }

function _isNativeReflectConstruct() { if (typeof Reflect === "undefined" || !Reflect.construct) return false; if (Reflect.construct.sham) return false; if (typeof Proxy === "function") return true; try { Boolean.prototype.valueOf.call(Reflect.construct(Boolean, [], function () {})); return true; } catch (e) { return false; } }

var ShinyRock = /*#__PURE__*/function (_MapObject) {
  (0, _inherits2.default)(ShinyRock, _MapObject);

  var _super = _createSuper(ShinyRock);

  function ShinyRock(scenemap, x, y, t_obj) {
    var _this;

    (0, _classCallCheck2.default)(this, ShinyRock);
    _this = _super.call(this, scenemap, x, y, t_obj);
    _this.engraved = false;
    _this.ichingframe = Math.floor(Math.random() * _this.scene.textures.get('iching').frameTotal);
    _this.progress = new _ProgressBar.default(_this.scene, _this.x + _this.width / 2, _this.y - _this.height, 32, 0.01);
    _this.machine = (0, _xstate.interpret)((0, _xstate.Machine)(ShinyRockMachineConfig, {
      actions: {
        addEngraving: function addEngraving() {
          _this.scene.add.image(_this.x + _this.width / 2, _this.y - _this.height / 2 * 0.9, 'iching', _this.ichingframe).setScale(0.155).setDepth(_this.depth + 1);

          _this.progress.destroy();

          _this.scene.tweens.add({
            targets: [_this.scene.cameras.main],
            zoom: "+=2",
            duration: 4000,
            yoyo: true,
            delay: 300
          });
        }
      },
      guards: {
        done: function done() {
          return _this.progress.done;
        }
      }
    }));

    _this.machine.start(); // new Tooltip(this.scene,this.x+this.width/2,this.y-this.height/2,this.width*1.5,"hold <Shift> to engrave")
    // this.scene.physics.world.addOverlap()
    //TODO breaks switching level
    // this.scene.events.on('create',()=>{
    //     this.scene.physics.world.addCollider(this,this.scene.player,()=>{
    //         this.progress.incrementProgress(0.2)
    //         console.log('hitittt')
    //         this.machine.send('HIT')
    //     })
    //     let cll = approachArea(this,this.scene.player,()=>{
    //         console.log("Approach!!")
    //         this.machine.send('APPROACHED')
    //         cll.destroy();
    //     })
    // });


    return _this;
  }

  return ShinyRock;
}(_MapObject2.default);

exports.default = ShinyRock;
var ShinyRockMachineConfig = {
  initial: 'alone',
  states: {
    alone: {
      on: {
        APPROACHED: 'selected'
      }
    },
    selected: {
      // entry: [''],
      on: {
        HIT: [{
          target: 'engraved',
          cond: 'done'
        }, 'selected']
      }
    },
    engraved: {
      type: 'final',
      entry: ['addEngraving']
    }
  }
};