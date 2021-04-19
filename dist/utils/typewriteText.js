"use strict";

var _interopRequireDefault = require("@babel/runtime/helpers/interopRequireDefault");

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.typewriteText = typewriteText;

var _regenerator = _interopRequireDefault(require("@babel/runtime/regenerator"));

var _asyncToGenerator2 = _interopRequireDefault(require("@babel/runtime/helpers/asyncToGenerator"));

var _awaitTimeout = _interopRequireDefault(require("./awaitTimeout"));

var _defaults = _interopRequireDefault(require("../defaults"));

require("phaser");

function typewriteText(text, twindow, scene, speed) {
  return new Promise(function (done, reject) {
    readChars(text, speed !== null && speed !== void 0 ? speed : _defaults.default.talkingSpeed, function (char) {
      twindow.appendMDText(char);
    }).then(function (v) {
      return done(undefined);
    });
  });
}

function readChars(_x, _x2, _x3) {
  return _readChars.apply(this, arguments);
}

function _readChars() {
  _readChars = (0, _asyncToGenerator2.default)( /*#__PURE__*/_regenerator.default.mark(function _callee(str, basedelay, cb) {
    var length, debugTime, i, char;
    return _regenerator.default.wrap(function _callee$(_context) {
      while (1) {
        switch (_context.prev = _context.next) {
          case 0:
            length = str.length;
            debugTime = 0;
            i = 0;

          case 3:
            if (!(i < length)) {
              _context.next = 18;
              break;
            }

            char = str[i];
            cb(char);

            if (!(char == ' ')) {
              _context.next = 12;
              break;
            }

            debugTime += basedelay * 1.7;
            _context.next = 10;
            return (0, _awaitTimeout.default)(basedelay * 1.7);

          case 10:
            _context.next = 15;
            break;

          case 12:
            debugTime += basedelay;
            _context.next = 15;
            return (0, _awaitTimeout.default)(basedelay);

          case 15:
            ++i;
            _context.next = 3;
            break;

          case 18:
            console.log("debugTime readChars ".concat(debugTime));

          case 19:
          case "end":
            return _context.stop();
        }
      }
    }, _callee);
  }));
  return _readChars.apply(this, arguments);
}
//# sourceMappingURL=typewriteText.js.map