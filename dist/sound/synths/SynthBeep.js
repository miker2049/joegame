"use strict";

var _interopRequireDefault = require("@babel/runtime/helpers/interopRequireDefault");

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.default = void 0;

var _classCallCheck2 = _interopRequireDefault(require("@babel/runtime/helpers/classCallCheck"));

var _createClass2 = _interopRequireDefault(require("@babel/runtime/helpers/createClass"));

var _defineProperty2 = _interopRequireDefault(require("@babel/runtime/helpers/defineProperty"));

var _default = /*#__PURE__*/function () {
  // synth: Tone.PolySynth
  // seq: Tone.Sequence
  function _default() {// this.synth = new Tone.PolySynth().toDestination();
    // this.seq = new Tone.Sequence((time, note) => {
    //     this.synth.triggerAttackRelease(note, 0.1, time);
    //     // subdivisions are given as subarrays
    // }, ["C4", ["E4", "D4", "E4"], "G4", ["A4", "G4"]])
    // Tone.Transport.start()

    (0, _classCallCheck2.default)(this, _default);
    (0, _defineProperty2.default)(this, "id", 'Arp');
    (0, _defineProperty2.default)(this, "volume", 0.75);
  }

  (0, _createClass2.default)(_default, [{
    key: "play",
    value: function play() {// console.log(this.synth)
      // console.log(Tone.Transport.state)
      // this.seq.start(0.05)
      // let now = Tone.now()
      // this.synth.triggerAttackRelease("C4", "16n")
      // this.synth.triggerAttackRelease("E4", "16n", now + 0.5)
      // this.synth.triggerAttackRelease("G4", "16n", now + 1)
      // this.synth.triggerAttackRelease("B4", "16n", now + 1.5)
    }
  }, {
    key: "setVolume",
    value: function setVolume() {}
  }]);
  return _default;
}();

exports.default = _default;
//# sourceMappingURL=SynthBeep.js.map