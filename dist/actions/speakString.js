"use strict";

var _interopRequireDefault = require("@babel/runtime/helpers/interopRequireDefault");

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.default = _default;

var _regenerator = _interopRequireDefault(require("@babel/runtime/regenerator"));

var _asyncToGenerator2 = _interopRequireDefault(require("@babel/runtime/helpers/asyncToGenerator"));

require("phaser");

var _awaitTimeout = _interopRequireDefault(require("../utils/awaitTimeout"));

var _defaults = _interopRequireDefault(require("../defaults"));

function _default(_x, _x2, _x3, _x4) {
  return _ref.apply(this, arguments);
}

function _ref() {
  _ref = (0, _asyncToGenerator2.default)( /*#__PURE__*/_regenerator.default.mark(function _callee2(str, char, speakFunc, speed) {
    var _str$match$length, _str$match;

    var words, wlength, ttime, _loop, i;

    return _regenerator.default.wrap(function _callee2$(_context3) {
      while (1) {
        switch (_context3.prev = _context3.next) {
          case 0:
            if (Phaser.Geom.Rectangle.ContainsPoint(char.scene.cameras.main.getBounds(), new Phaser.Geom.Point(char.x, char.y))) {
              _context3.next = 2;
              break;
            }

            return _context3.abrupt("return");

          case 2:
            words = str.split(' ');
            wlength = words.length;
            ttime = (speed !== null && speed !== void 0 ? speed : _defaults.default.talkingSpeed) * str.replace(' ', '').length + ((_str$match$length = (_str$match = str.match(/\s/g)) === null || _str$match === void 0 ? void 0 : _str$match.length) !== null && _str$match$length !== void 0 ? _str$match$length : 0) * (_defaults.default.talkingSpeed * 1.7);
            _loop = /*#__PURE__*/_regenerator.default.mark(function _loop(i) {
              var syllable, randArr;
              return _regenerator.default.wrap(function _loop$(_context2) {
                while (1) {
                  switch (_context2.prev = _context2.next) {
                    case 0:
                      syllable = syllableCount(words[i]);
                      randArr = randArrayAndSum(syllable); // for(let j = 0; j< syllable;j++){
                      // }

                      randArr[0].forEach( /*#__PURE__*/function () {
                        var _ref2 = (0, _asyncToGenerator2.default)( /*#__PURE__*/_regenerator.default.mark(function _callee(val) {
                          return _regenerator.default.wrap(function _callee$(_context) {
                            while (1) {
                              switch (_context.prev = _context.next) {
                                case 0:
                                  speakFunc();
                                  _context.next = 3;
                                  return (0, _awaitTimeout.default)(val / randArr[1] * (words[i].length * _defaults.default.talkingSpeed));

                                case 3:
                                case "end":
                                  return _context.stop();
                              }
                            }
                          }, _callee);
                        }));

                        return function (_x5) {
                          return _ref2.apply(this, arguments);
                        };
                      }());
                      _context2.next = 5;
                      return (0, _awaitTimeout.default)(_defaults.default.talkingSpeed * 1.7);

                    case 5:
                    case "end":
                      return _context2.stop();
                  }
                }
              }, _loop);
            });
            i = 0;

          case 7:
            if (!(i < wlength)) {
              _context3.next = 12;
              break;
            }

            return _context3.delegateYield(_loop(i), "t0", 9);

          case 9:
            i++;
            _context3.next = 7;
            break;

          case 12:
            return _context3.abrupt("return");

          case 13:
          case "end":
            return _context3.stop();
        }
      }
    }, _callee2);
  }));
  return _ref.apply(this, arguments);
}

function randArrayAndSum(length) {
  var sum = 0;
  var arr = [];

  for (var i = 0; i < length; i++) {
    arr.push(Math.random());
  }

  return [arr, arr.reduce(function (pr, val) {
    return pr + val;
  })];
} // https://dl.acm.org/doi/10.1145/10563.10583
//  Each vowel (a, e, i, o, u, y) in a word counts as one syllable
//  subject to the following sub-rules:
// - Ignore final -ES, -ED, E (except for -LE)
// - Words of three letters or less count as one syllable
// - Consecutive vowels count as one syllable.
//
// https://stackoverflow.com/questions/5686483/how-to-compute-number-of-syllables-in-a-word-in-javascript


function syllableCount(word) {
  var _word$match$length, _word$match;

  word = word.toLowerCase();

  if (word.length <= 3) {
    return 1;
  }

  word = word.replace(/(?:[^laeiouy]es|ed|[^laeiouy]e)$/, '');
  word = word.replace(/^y/, '');
  return (_word$match$length = (_word$match = word.match(/[aeiouy]{1,2}/g)) === null || _word$match === void 0 ? void 0 : _word$match.length) !== null && _word$match$length !== void 0 ? _word$match$length : 1;
}
//# sourceMappingURL=speakString.js.map