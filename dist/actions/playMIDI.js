"use strict";

var _interopRequireDefault = require("@babel/runtime/helpers/interopRequireDefault");

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.default = _default;

var _regenerator = _interopRequireDefault(require("@babel/runtime/regenerator"));

var _asyncToGenerator2 = _interopRequireDefault(require("@babel/runtime/helpers/asyncToGenerator"));

var _jsSynthesizer = _interopRequireDefault(require("js-synthesizer"));

function _default(_x, _x2) {
  return _ref.apply(this, arguments);
}

function _ref() {
  _ref = (0, _asyncToGenerator2.default)( /*#__PURE__*/_regenerator.default.mark(function _callee(midiURL, sfURL) {
    var midiBuff, sfontBuffer, context, synth, audioNode;
    return _regenerator.default.wrap(function _callee$(_context) {
      while (1) {
        switch (_context.prev = _context.next) {
          case 0:
            _context.next = 2;
            return fetch(midiURL);

          case 2:
            midiBuff = _context.sent;
            _context.next = 5;
            return fetch(sfURL);

          case 5:
            sfontBuffer = _context.sent;
            context = new AudioContext();
            _context.next = 9;
            return context.audioWorklet.addModule('/joegame/libfluidsynth-2.1.3.js');

          case 9:
            _context.next = 11;
            return context.audioWorklet.addModule('/joegame/js-synthesizer.worklet.js');

          case 11:
            // Create the synthesizer instance for AudioWorkletNode
            synth = new _jsSynthesizer.default.AudioWorkletNodeSynthesizer();
            synth.init(context.sampleRate); // You must create AudioWorkletNode before using other methods
            // (This is because the message port is not available until the
            // AudioWorkletNode is created)

            audioNode = synth.createAudioNode(context);
            audioNode.connect(context.destination); // or another node...
            // After node creation, you can use Synthesizer methods

            _context.t0 = synth;
            _context.next = 18;
            return sfontBuffer.arrayBuffer();

          case 18:
            _context.t1 = _context.sent;
            _context.next = 21;
            return _context.t0.loadSFont.call(_context.t0, _context.t1);

          case 21:
            _context.t2 = synth;
            _context.next = 24;
            return midiBuff.arrayBuffer();

          case 24:
            _context.t3 = _context.sent;
            _context.next = 27;
            return _context.t2.addSMFDataToPlayer.call(_context.t2, _context.t3);

          case 27:
            return _context.abrupt("return", synth.playPlayer());

          case 28:
          case "end":
            return _context.stop();
        }
      }
    }, _callee);
  }));
  return _ref.apply(this, arguments);
}