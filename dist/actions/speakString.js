"use strict";

var _interopRequireDefault = require("@babel/runtime/helpers/interopRequireDefault");

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.default = _default;

var _regenerator = _interopRequireDefault(require("@babel/runtime/regenerator"));

var _asyncToGenerator2 = _interopRequireDefault(require("@babel/runtime/helpers/asyncToGenerator"));

require("phaser");

var _lodash = require("lodash");

var _awaitTimeout = _interopRequireDefault(require("../utils/awaitTimeout"));

var _defaults = _interopRequireDefault(require("../defaults"));

var _syllableCount = _interopRequireDefault(require("../utils/syllableCount"));

var _hashToArr = _interopRequireDefault(require("../utils/hashToArr"));

var _Debug = require("tone/build/esm/core/util/Debug");

function _default(_x, _x2, _x3, _x4) {
  return _ref.apply(this, arguments);
}

function _ref() {
  _ref = (0, _asyncToGenerator2.default)( /*#__PURE__*/_regenerator.default.mark(function _callee(str, char, speakFunc, speed) {
    var words, wlength, i, syllable, randArr, numbers, j, delay, _numbers$j$, _numbers$j$2, vAndp;

    return _regenerator.default.wrap(function _callee$(_context) {
      while (1) {
        switch (_context.prev = _context.next) {
          case 0:
            // if (!(Phaser.Geom.Rectangle.ContainsPoint(char.scene.cameras.main.getBounds(), new Phaser.Geom.Point(char.x, char.y)))) return
            words = str.split(' ');
            wlength = words.length; // const ttime = ((speed ?? globalDefaults.talkingSpeed) * str.replace(' ', '').length) +
            //     ((str.match(/\s/g)?.length ?? 0) * (globalDefaults.talkingSpeed * 1.7))

            i = 0;

          case 3:
            if (!(i < wlength)) {
              _context.next = 22;
              break;
            }

            // console.log(hashCode(words[i]))
            syllable = (0, _syllableCount.default)(words[i]);
            randArr = randArrayAndSum(syllable); // need two indexes for each syllable, for vowel (buff) and rate (pitch)
            // TODO remove lodash dependency!!!

            numbers = (0, _lodash.chunk)((0, _hashToArr.default)(words[i], syllable * 2), 2);
            (0, _Debug.assert)(randArr[0].length === numbers.length, 'not same laengthss');
            j = 0;

          case 9:
            if (!(j < randArr[0].length)) {
              _context.next = 17;
              break;
            }

            delay = randArr[0][j] / randArr[1] * (words[i].length * _defaults.default.talkingSpeed);

            if (char.scene.cameras.main.worldView.contains(char.x, char.y) == true) {
              vAndp = getVolAndPanFromDistance(char.scene.cameras.main.worldView.centerX, char.scene.cameras.main.worldView.centerX, char.x, char.y, char.scene.cameras.main.worldView.width);
              speakFunc({
                inst: 'talking',
                buff: (_numbers$j$ = numbers[j][0]) !== null && _numbers$j$ !== void 0 ? _numbers$j$ : undefined,
                rate: (_numbers$j$2 = numbers[j][1]) !== null && _numbers$j$2 !== void 0 ? _numbers$j$2 : undefined,
                vol: vAndp[0],
                pan: vAndp[1]
              });
            }

            _context.next = 14;
            return (0, _awaitTimeout.default)(delay);

          case 14:
            j++;
            _context.next = 9;
            break;

          case 17:
            _context.next = 19;
            return (0, _awaitTimeout.default)(_defaults.default.talkingSpeed * 1.7);

          case 19:
            i++;
            _context.next = 3;
            break;

          case 22:
            return _context.abrupt("return");

          case 23:
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

function clump(arr, n) {
  if (arr.length < n) return [arr];
  var i = 0,
      out = [];
}

function getVolAndPanFromDistance(playerX, playerY, charX, charY, cameraWidth) {
  var difference = charX - playerX;
  var distance = Math.sqrt(difference ^ 2 + (charY - playerY) ^ 2);
  var mod = difference > 0 ? -1 : 1;
  var pan = difference / (cameraWidth / 2);
  var vol = distance / (cameraWidth / 2);
  return [vol, pan];
}
//# sourceMappingURL=speakString.js.map