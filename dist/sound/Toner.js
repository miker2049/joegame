"use strict";

var _interopRequireWildcard = require("@babel/runtime/helpers/interopRequireWildcard");

var _interopRequireDefault = require("@babel/runtime/helpers/interopRequireDefault");

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.default = void 0;

var _classCallCheck2 = _interopRequireDefault(require("@babel/runtime/helpers/classCallCheck"));

var _createClass2 = _interopRequireDefault(require("@babel/runtime/helpers/createClass"));

var Tone = _interopRequireWildcard(require("tone"));

var _Gong = _interopRequireDefault(require("./synths/Gong"));

var _SynthBeep = _interopRequireDefault(require("./synths/SynthBeep"));

var _Talking = require("./synths/Talking");

var _Walk = _interopRequireDefault(require("./synths/Walk"));

var Toner = /*#__PURE__*/function () {
  function Toner(context) {
    (0, _classCallCheck2.default)(this, Toner);
    this.context = context;
    Tone.setContext(this.context);
    this.instruments = new Map();
    this.instruments.set('arp', new _SynthBeep.default());
    this.instruments.set('gong', new _Gong.default());
    this.instruments.set('walk', new _Walk.default());
    Tone.setContext(this.context);
    this.instruments.set('talking', new _Talking.Talking());
    Tone.setContext(this.context);
    Tone.start();
    Tone.Transport.start();
  }

  (0, _createClass2.default)(Toner, [{
    key: "play",
    value: function play(config) {
      var found = this.instruments.get(config.inst);
      if (found && this.context.state === 'running') found.play(config);
    }
  }]);
  return Toner;
}();

exports.default = Toner;
//# sourceMappingURL=Toner.js.map