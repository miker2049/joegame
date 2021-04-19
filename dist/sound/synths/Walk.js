"use strict";

var _interopRequireWildcard = require("@babel/runtime/helpers/interopRequireWildcard");

var _interopRequireDefault = require("@babel/runtime/helpers/interopRequireDefault");

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.default = void 0;

var _classCallCheck2 = _interopRequireDefault(require("@babel/runtime/helpers/classCallCheck"));

var _createClass2 = _interopRequireDefault(require("@babel/runtime/helpers/createClass"));

var _defineProperty2 = _interopRequireDefault(require("@babel/runtime/helpers/defineProperty"));

var Tone = _interopRequireWildcard(require("tone"));

var _default = /*#__PURE__*/function () {
  function _default() {
    var _this = this;

    (0, _classCallCheck2.default)(this, _default);
    (0, _defineProperty2.default)(this, "id", 'walk');
    (0, _defineProperty2.default)(this, "volume", 0.75);
    (0, _defineProperty2.default)(this, "ready", false);
    this.synth = new Tone.Player().toDestination();
    Tone.Offline(function (transport) {
      var pluck = new Tone.PluckSynth({
        resonance: 0.1,
        volume: -18
      }).toDestination();
      var tnow = transport.currentTime;
      pluck.triggerAttack("A3", tnow);
      pluck.triggerAttack("G3", tnow + 0.125 / 4);
    }, 0.25).then(function (buff) {
      _this.ready = true;
      _this.synth.buffer = buff;
    });
  }

  (0, _createClass2.default)(_default, [{
    key: "play",
    value: function play() {
      if (this.ready) {
        // this.synth.stop()
        this.synth.start();
      }
    }
  }, {
    key: "setVolume",
    value: function setVolume() {}
  }]);
  return _default;
}();

exports.default = _default;
//# sourceMappingURL=Walk.js.map