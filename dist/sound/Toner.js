"use strict";

var _interopRequireDefault = require("@babel/runtime/helpers/interopRequireDefault");

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.default = void 0;

var _classCallCheck2 = _interopRequireDefault(require("@babel/runtime/helpers/classCallCheck"));

var _createClass2 = _interopRequireDefault(require("@babel/runtime/helpers/createClass"));

var _SynthBeep = _interopRequireDefault(require("./SynthBeep"));

var Toner = /*#__PURE__*/function () {
  function Toner() {
    (0, _classCallCheck2.default)(this, Toner);
    this.instruments = new Map();
    this.instruments.set('arp', new _SynthBeep.default());
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