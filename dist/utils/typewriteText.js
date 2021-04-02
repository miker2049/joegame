"use strict";

var _interopRequireDefault = require("@babel/runtime/helpers/interopRequireDefault");

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.default = _default;

var _regenerator = _interopRequireDefault(require("@babel/runtime/regenerator"));

var _asyncToGenerator2 = _interopRequireDefault(require("@babel/runtime/helpers/asyncToGenerator"));

require("phaser");

function _default(text, twindow, scene, speed) {
  return new Promise(function (done, reject) {
    readChars(text, speed !== null && speed !== void 0 ? speed : 20, function (char) {
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
    var length, i, char;
    return _regenerator.default.wrap(function _callee$(_context) {
      while (1) {
        switch (_context.prev = _context.next) {
          case 0:
            length = str.length;
            i = 0;

          case 2:
            if (!(i < length)) {
              _context.next = 15;
              break;
            }

            char = str[i];
            cb(char);

            if (!(char == ' ')) {
              _context.next = 10;
              break;
            }

            _context.next = 8;
            return timeout(basedelay * 1.7);

          case 8:
            _context.next = 12;
            break;

          case 10:
            _context.next = 12;
            return timeout(basedelay);

          case 12:
            ++i;
            _context.next = 2;
            break;

          case 15:
          case "end":
            return _context.stop();
        }
      }
    }, _callee);
  }));
  return _readChars.apply(this, arguments);
}

function timeout(ms) {
  return new Promise(function (resolve) {
    return setTimeout(resolve, ms);
  });
}
//# sourceMappingURL=typewriteText.js.map