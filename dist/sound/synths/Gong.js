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
    (0, _defineProperty2.default)(this, "id", 'Gong');
    (0, _defineProperty2.default)(this, "volume", 0.75);
    this.synth = new _tone.Player("https://tonejs.github.io/audio/berklee/gong_1.mp3").toDestination();
  }

  (0, _createClass2.default)(_default, [{
    key: "play",
    value: function play() {
      var _this = this;

      (0, _tone.loaded)().then(function () {
        console.log(_this.synth);

        _this.synth.start(0);
      });
    }
  }, {
    key: "setVolume",
    value: function setVolume() {}
  }]);
  return _default;
}();

exports.default = _default;
//# sourceMappingURL=Gong.js.map