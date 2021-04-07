"use strict";

var _interopRequireDefault = require("@babel/runtime/helpers/interopRequireDefault");

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.default = void 0;

var _classCallCheck2 = _interopRequireDefault(require("@babel/runtime/helpers/classCallCheck"));

var _createClass2 = _interopRequireDefault(require("@babel/runtime/helpers/createClass"));

var _defineProperty2 = _interopRequireDefault(require("@babel/runtime/helpers/defineProperty"));

var _tone = require("tone");

var _default = /*#__PURE__*/function () {
  function _default() {
    (0, _classCallCheck2.default)(this, _default);
    (0, _defineProperty2.default)(this, "id", 'Arp');
    (0, _defineProperty2.default)(this, "volume", 0.75);
    this.synth = new _tone.Synth().toDestination();
  }

  (0, _createClass2.default)(_default, [{
    key: "play",
    value: function play() {
      var tnow = (0, _tone.now)();
      this.synth.triggerAttackRelease("C4", "8n", tnow);
      this.synth.triggerAttackRelease("E4", "8n", tnow + 0.5);
      this.synth.triggerAttackRelease("G4", "8n", tnow + 1);
      this.synth.triggerAttackRelease("B4", "8n", tnow + 1.5);
    }
  }, {
    key: "setVolume",
    value: function setVolume() {}
  }]);
  return _default;
}();

exports.default = _default;
//# sourceMappingURL=SynthBeep.js.map