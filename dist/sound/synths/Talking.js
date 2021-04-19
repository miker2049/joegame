"use strict";

var _interopRequireWildcard = require("@babel/runtime/helpers/interopRequireWildcard");

var _interopRequireDefault = require("@babel/runtime/helpers/interopRequireDefault");

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.Talking = void 0;

var _regenerator = _interopRequireDefault(require("@babel/runtime/regenerator"));

var _asyncToGenerator2 = _interopRequireDefault(require("@babel/runtime/helpers/asyncToGenerator"));

var _classCallCheck2 = _interopRequireDefault(require("@babel/runtime/helpers/classCallCheck"));

var _createClass2 = _interopRequireDefault(require("@babel/runtime/helpers/createClass"));

var _defineProperty2 = _interopRequireDefault(require("@babel/runtime/helpers/defineProperty"));

var Tone = _interopRequireWildcard(require("tone"));

var envelope = {
  attack: 0.01,
  sustain: 1.0,
  decay: 0.01
};
var root = 261.63; // G4

var fourth = 349.23;
var third = 329.63;
var fifth = 392.;
var sixth = 440.;
var maj7 = 493.88;
var oct = 523.25; // const intervals = [root, fifth, fifth, third, maj7, fourth, sixth, oct]

var intervals = [root, fifth];
var VOICES = 8;

var Talking = /*#__PURE__*/function () {
  function Talking() {
    var _this = this;

    (0, _classCallCheck2.default)(this, Talking);
    (0, _defineProperty2.default)(this, "id", 'talking');
    (0, _defineProperty2.default)(this, "volume", 0.75);
    (0, _defineProperty2.default)(this, "ready", false);
    (0, _defineProperty2.default)(this, "currSynth", 0);
    this.buffs = [];
    this.synths = [];

    for (var i = 0; i < VOICES; i++) {
      this.synths.push(function () {
        var panner = new Tone.Panner(0).toDestination();
        return {
          player: new Tone.Player({
            fadeIn: 0.015,
            fadeOut: 0.015
          }).connect(panner),
          panner: panner
        };
      }());
    }

    this.init().then(function () {
      return _this.ready = true;
    });
  }

  (0, _createClass2.default)(Talking, [{
    key: "play",
    value: function play(config) {
      if (this.ready) {
        var _config$vol, _this$buffs;

        // if (config.pan) this.panner.set({ pan: config.pan })
        // this.panner.set({ pan: (Math.random() * 2) - 1 })
        this.synths[this.currSynth].panner.set({
          pan: config.pan
        });
        this.synths[this.currSynth].player.volume.value = ((_config$vol = config.vol) !== null && _config$vol !== void 0 ? _config$vol : 0.5) * -18;
        var buffer = (_this$buffs = this.buffs[config.buff % this.buffs.length]) !== null && _this$buffs !== void 0 ? _this$buffs : this.buffs[Math.floor(Math.random() * this.buffs.length)];
        this.synths[this.currSynth].player.buffer = buffer;
        var interval = config.rate ? intervals[config.rate % intervals.length] / root : intervals[Math.floor(Math.random() * intervals.length)] / root;
        this.synths[this.currSynth].player.playbackRate = interval;
        console.log(config.rate, buffer, interval);
        this.synths[this.currSynth].player.start(Tone.now() + 0.01);
        this.currSynth = (this.currSynth + 1) % VOICES;
      }
    }
  }, {
    key: "setVolume",
    value: function setVolume() {}
  }, {
    key: "createBuff",
    value: function () {
      var _createBuff = (0, _asyncToGenerator2.default)( /*#__PURE__*/_regenerator.default.mark(function _callee(cb) {
        return _regenerator.default.wrap(function _callee$(_context) {
          while (1) {
            switch (_context.prev = _context.next) {
              case 0:
                _context.next = 2;
                return Tone.Offline(cb, 0.25);

              case 2:
                return _context.abrupt("return", _context.sent);

              case 3:
              case "end":
                return _context.stop();
            }
          }
        }, _callee);
      }));

      function createBuff(_x) {
        return _createBuff.apply(this, arguments);
      }

      return createBuff;
    }()
  }, {
    key: "init",
    value: function () {
      var _init = (0, _asyncToGenerator2.default)( /*#__PURE__*/_regenerator.default.mark(function _callee2() {
        var _this2 = this;

        var vowels;
        return _regenerator.default.wrap(function _callee2$(_context2) {
          while (1) {
            switch (_context2.prev = _context2.next) {
              case 0:
                // https://soundbridge.io/formants-vowel-sounds/
                vowels = [[570, 840, 2410, 60], [300, 870, 2240, 60], [440, 1020, 2240, 60], [730, 1090, 2440, 60], [520, 1190, 2390, 60], [490, 1350, 1690, 60], [660, 1720, 2410, 60], [530, 1840, 2480, 60], [390, 1990, 2550, 60], [270, 2290, 3010, 60]];
                Promise.all(vowels.map(function (args) {
                  return _this2.createBuff(function (_transport) {
                    _this2.vowelUgen(args[0], args[1], args[2], Tone.Frequency(args[3], "midi").toFrequency());
                  });
                })).then(function (buffs) {
                  return _this2.buffs = buffs;
                });

              case 2:
              case "end":
                return _context2.stop();
            }
          }
        }, _callee2);
      }));

      function init() {
        return _init.apply(this, arguments);
      }

      return init;
    }()
  }, {
    key: "vowelUgen",
    value: function vowelUgen(f1, f2, f3, pitch) {
      // https://github.com/benfordslaw/vowel-sound-generator/blob/main/sketch.js
      var vol = new Tone.Volume(0).toDestination();
      var filtf1 = new Tone.Filter(f1, "bandpass").connect(vol);
      filtf1.Q.value = 5.0;
      var filtf2 = new Tone.Filter(f2, "bandpass").connect(vol);
      filtf2.Q.value = 13.0;
      var filtf3 = new Tone.Filter(f3, "bandpass").connect(vol);
      filtf3.Q.value = 14.0;
      var ampEnv = new Tone.AmplitudeEnvelope({
        attack: 0.1,
        decay: 0.2,
        sustain: 1.0,
        release: 0.8
      }).fan(filtf1, filtf2, filtf3); //noise adds a little natural-ness

      var noi = new Tone.Noise({
        type: "pink",
        volume: -16
      }).connect(ampEnv).start();
      var osc = new Tone.Oscillator({
        type: "sawtooth",
        frequency: pitch,
        volume: -8
      }).connect(ampEnv).start();
      ampEnv.triggerAttackRelease("8t");
    }
  }]);
  return Talking;
}();

exports.Talking = Talking;
//# sourceMappingURL=Talking.js.map