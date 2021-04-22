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

var _syllableCount = require("../utils/syllableCount");

var _hashToArr = require("../utils/hashToArr");

var _Debug = require("tone/build/esm/core/util/Debug");

var _getVolPanFromDist = require("../utils/getVolPanFromDist");

function _default(_x, _x2, _x3, _x4) {
  return _ref.apply(this, arguments);
}

function _ref() {
  _ref = (0, _asyncToGenerator2.default)( /*#__PURE__*/_regenerator.default.mark(function _callee(str, char, speakFunc, speed) {
    var words, wlength, i, syllable, randArr, numbers, j, delay, loc, _numbers$j$, _numbers$j$2, vAndp;

    return _regenerator.default.wrap(function _callee$(_context) {
      while (1) {
        switch (_context.prev = _context.next) {
          case 0:
            // if (!(Phaser.Geom.Rectangle.ContainsPoint(char.scene.cameras.main.getBounds(), new Phaser.Geom.Point(char.x, char.y)))) return
            words = str.replace(/[^A-Za-z0-9]/g, ' ').split(' ');
            wlength = words.length; // const ttime = ((speed ?? globalDefaults.talkingSpeed) * str.replace(' ', '').length) +
            //     ((str.match(/\s/g)?.length ?? 0) * (globalDefaults.talkingSpeed * 1.7))

            i = 0;

          case 3:
            if (!(i < wlength)) {
              _context.next = 25;
              break;
            }

            // console.log(hashCode(words[i]))
            syllable = (0, _syllableCount.syllableCount)(words[i]);

            if (!(words[i].length > 0)) {
              _context.next = 22;
              break;
            }

            randArr = randArrayAndSum(syllable); // need two indexes for each syllable, for vowel (buff) and rate (pitch)
            // TODO remove lodash dependency!!!

            numbers = chunk2((0, _hashToArr.hashToArr)(words[i], syllable * 2), 2);
            (0, _Debug.assert)((0, _hashToArr.hashToArr)(words[i], syllable * 2).length == syllable * 2, "hashtoArr returns the right word:\"".concat(words[i], "\"  ").concat(syllable, " AND ").concat((0, _hashToArr.hashToArr)(words[i], syllable * 2)));
            (0, _Debug.assert)(randArr[0].length === numbers.length, "not same laengthss ".concat(words[i], " ").concat(randArr[0].length, "  ").concat(numbers.length));
            j = 0;

          case 11:
            if (!(j < randArr[0].length)) {
              _context.next = 20;
              break;
            }

            delay = randArr[0][j] / randArr[1] * (words[i].length * _defaults.default.talkingSpeed);
            loc = {
              x: char.x || char.scene.cameras.main.worldView.centerX,
              y: char.y || char.scene.cameras.main.worldView.centerY
            };

            if (char.scene.cameras.main.worldView.contains(loc.x, loc.y) == true) {
              vAndp = (0, _getVolPanFromDist.getVolAndPanFromDistance)(char.scene.cameras.main.worldView.centerX, char.scene.cameras.main.worldView.centerX, loc.x, loc.y, char.scene.cameras.main.worldView.width);
              speakFunc({
                inst: 'talking',
                buff: (_numbers$j$ = numbers[j][0]) !== null && _numbers$j$ !== void 0 ? _numbers$j$ : undefined,
                rate: (_numbers$j$2 = numbers[j][1]) !== null && _numbers$j$2 !== void 0 ? _numbers$j$2 : undefined,
                vol: vAndp[0],
                pan: vAndp[1]
              });
            }

            _context.next = 17;
            return (0, _awaitTimeout.default)(delay);

          case 17:
            j++;
            _context.next = 11;
            break;

          case 20:
            _context.next = 22;
            return (0, _awaitTimeout.default)(_defaults.default.talkingSpeed * 2);

          case 22:
            i++;
            _context.next = 3;
            break;

          case 25:
            return _context.abrupt("return");

          case 26:
          case "end":
            return _context.stop();
        }
      }
    }, _callee);
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
}

function chunk2(arr, n) {
  var out = [];

  for (var i = 0; i < arr.length; i += n) {
    out.push(arr.slice(i, i + n));
  }

  return out;
}
//# sourceMappingURL=speakString.js.map