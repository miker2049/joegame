"use strict";

var _interopRequireDefault = require("@babel/runtime/helpers/interopRequireDefault");

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.default = void 0;

var _classCallCheck2 = _interopRequireDefault(require("@babel/runtime/helpers/classCallCheck"));

var _createClass2 = _interopRequireDefault(require("@babel/runtime/helpers/createClass"));

var _Gong = _interopRequireDefault(require("./synths/Gong"));

var _SynthBeep = _interopRequireDefault(require("./synths/SynthBeep"));

var _Walk = _interopRequireDefault(require("./synths/Walk"));

var Toner = /*#__PURE__*/function () {
  function Toner() {
    (0, _classCallCheck2.default)(this, Toner);
    this.instruments = new Map();
    this.instruments.set('arp', new _SynthBeep.default());
    this.instruments.set('gong', new _Gong.default());
    this.instruments.set('walk', new _Walk.default());
  }

  (0, _createClass2.default)(Toner, [{
    key: "play",
    value: function play(inst) {
      var found = this.instruments.get(inst);
      if (found) found.play();
    }
  }]);
  return Toner;
}();

exports.default = Toner;
//# sourceMappingURL=Toner.js.map