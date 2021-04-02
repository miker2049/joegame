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

var _Character2 = require("./Character");

var _xstate = require("xstate");

var _NPCMachine = _interopRequireDefault(require("./NPCMachine"));

var _joegameData = require("./joegameData");

var _SpeechBox = _interopRequireDefault(require("./SpeechBox"));

function _createSuper(Derived) { var hasNativeReflectConstruct = _isNativeReflectConstruct(); return function _createSuperInternal() { var Super = (0, _getPrototypeOf2.default)(Derived), result; if (hasNativeReflectConstruct) { var NewTarget = (0, _getPrototypeOf2.default)(this).constructor; result = Reflect.construct(Super, arguments, NewTarget); } else { result = Super.apply(this, arguments); } return (0, _possibleConstructorReturn2.default)(this, result); }; }

function _isNativeReflectConstruct() { if (typeof Reflect === "undefined" || !Reflect.construct) return false; if (Reflect.construct.sham) return false; if (typeof Proxy === "function") return true; try { Boolean.prototype.valueOf.call(Reflect.construct(Boolean, [], function () {})); return true; } catch (e) { return false; } }

var NPC = /*#__PURE__*/function (_Character) {
  (0, _inherits2.default)(NPC, _Character);

  var _super = _createSuper(NPC);

  function NPC(scene) {
    var _this;

    var x = arguments.length > 1 && arguments[1] !== undefined ? arguments[1] : 0;
    var y = arguments.length > 2 && arguments[2] !== undefined ? arguments[2] : 0;
    var config = arguments.length > 3 ? arguments[3] : undefined;
    var interests = arguments.length > 4 ? arguments[4] : undefined;
    (0, _classCallCheck2.default)(this, NPC);
    _this = _super.call(this, scene, x, y, config);
    _this.interests = interests;
    _this.currInterest = 0; // const machOpts = machineOptions(this);

    _this.patience = 1000;
    _this.speed = 12;

    var mach = _NPCMachine.default.withContext({
      char: (0, _assertThisInitialized2.default)(_this),
      interestCounter: 1,
      additionalAvoid: {
        x: 1,
        y: 1
      },
      auto: true,
      patience: 2000,
      interests: interests
    });

    _this.machine = (0, _xstate.interpret)(mach);
    _this.auto = true; // this.setOrigin(0.5,0.85);
    // let thisName: string;
    // if (name){
    //     thisName = name;
    //     this.name = name;
    // } else {
    //     thisName = nameList[Math.floor(nameList.length * Math.random())]
    //     this.name = thisName;
    // }

    _this.label.text = _this.name;

    _this.scene.npcGroup.add((0, _assertThisInitialized2.default)(_this), true);

    _this.scene.npcs.set(_this.name, (0, _assertThisInitialized2.default)(_this));

    _this.speechbox = new _SpeechBox.default(_this.scene, (0, _assertThisInitialized2.default)(_this));

    _this.add(_this.speechbox); //TODO this will be changed to pushable on change to stable 3.50


    _this.body.setImmovable(true); // this.body.setPushable(false)


    _this.scene.game.events.once('levelloaded', function () {
      _this.initColliders();

      _this.machine.start();
    });

    _this.scene.events.once('levelclosed', function () {
      // this.initColliders();
      // this.machine.start();
      console.log("this should stop machine");

      _this.machine.stop();
    }); // this.scene.events.once(Phaser.Core.Events.DESTROY,()=>{
    //     this.machine.stop()
    // })
    // this.scene.events.once(Phaser.Scenes.Events.DESTROY,()=>{
    //     console.log("this should be stopping NPC machines!")
    //     this.machine.stop()
    // })
    // this.scene.events.once(Phaser.Scenes.Events.PAUSE,()=>{
    //     this.machine.stop()
    // })


    return _this;
  }

  (0, _createClass2.default)(NPC, [{
    key: "idlingAction",
    value: function idlingAction() {
      var _this2 = this;

      if (this.auto) {
        this.scene.time.addEvent({
          callback: function callback() {
            _this2.currInterest = (_this2.currInterest + 1) % _this2.interests.length;

            _this2.machine.send('MOVE_THOUGHT', {
              location: {
                x: _this2.interests[_this2.currInterest].x,
                y: _this2.interests[_this2.currInterest].y,
                finalFacing: _joegameData.Dir.north
              }
            });
          },
          delay: this.patience
        });
      }
    }
  }, {
    key: "initColliders",
    value: function initColliders() {// TODO breaks switching level
      // this.scene.physics.add.collider(this, this.scene.map.mainLayer, (npc,wall)=>{
      //     this.machine.send('WALL_BUMP');
      //     // console.log('WALL_BUMP');
      // });
      // // with the player
      // this.scene.physics.add.collider(this,this.scene.player,(npc: NPC, player: Player)=>{
      //     // console.log('PLAYER_BUMP');
      //     // console.log(npc);
      //     // player.stopMove();
      //     npc.machine.send({type:'BUMP', sprite: player})
      // });
      // // with each other
      // this.scene.physics.add.collider(this,this.scene.npcGroup,(npc: NPC, npc_: NPC)=>{
      //     npc_.machine.send({type:'BUMP', sprite: npc})
      //     npc.machine.send({type:'BUMP', sprite: npc_})
      // });
    }
  }]);
  return NPC;
}(_Character2.Character);

exports.default = NPC;
//# sourceMappingURL=NPC.js.map