"use strict";

var _interopRequireDefault = require("@babel/runtime/helpers/interopRequireDefault");

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.default = void 0;

var _regenerator = _interopRequireDefault(require("@babel/runtime/regenerator"));

var _asyncToGenerator2 = _interopRequireDefault(require("@babel/runtime/helpers/asyncToGenerator"));

var _classCallCheck2 = _interopRequireDefault(require("@babel/runtime/helpers/classCallCheck"));

var _createClass2 = _interopRequireDefault(require("@babel/runtime/helpers/createClass"));

var _asyncIterator2 = _interopRequireDefault(require("@babel/runtime/helpers/asyncIterator"));

var _NPCMachine = require("./NPCMachine");

var _randomInterestSet = _interopRequireDefault(require("../utils/randomInterestSet"));

var _xstate = require("xstate");

var _defaults = _interopRequireDefault(require("../defaults"));

var urlRegex = /((([A-Za-z]{3,9}:(?:\/\/)?)(?:[\-;:&=\+\$,\w]+@)?[A-Za-z0-9\.\-]+|(?:www\.|[\-;:&=\+\$,\w]+@)[A-Za-z0-9\.\-]+)((?:\/[\+~%\/\.\w\-_]*)?\??(?:[\-\+=&;%@\.\w_]*)#?(?:[\.\!\/\\\w]*))?)/g;

var TweetConvo = /*#__PURE__*/function () {
  // box:
  function TweetConvo(chars, convo, users, level) {
    (0, _classCallCheck2.default)(this, TweetConvo);
    this.chars = chars;
    this.convo = convo;
    this.users = users; // this.chars[0].

    this.chars.forEach(function (char) {
      if (true) {
        // if ((() => Math.random())() > 0.5) {
        var mach = (0, _NPCMachine.createNPCMachine)(char, level.map.tileWidth, level.pathfinder, (0, _randomInterestSet.default)(level.map, 4, {
          x: char.x,
          y: char.y
        }));
        var intt = (0, _xstate.interpret)(mach); // intt.start()
      }
    }); // this.chars.forEach(char=>{
    //     char.scene.physics.add.overlap(char,level.player,)
    // })
  }

  (0, _createClass2.default)(TweetConvo, [{
    key: "runConvo",
    value: function () {
      var _runConvo = (0, _asyncToGenerator2.default)( /*#__PURE__*/_regenerator.default.mark(function _callee() {
        var _this = this;

        var _iteratorNormalCompletion, _didIteratorError, _iteratorError, _loop, _iterator, _step, _value;

        return _regenerator.default.wrap(function _callee$(_context2) {
          while (1) {
            switch (_context2.prev = _context2.next) {
              case 0:
                _iteratorNormalCompletion = true;
                _didIteratorError = false;
                _context2.prev = 2;
                _loop = /*#__PURE__*/_regenerator.default.mark(function _loop() {
                  var conv, charIndex, text;
                  return _regenerator.default.wrap(function _loop$(_context) {
                    while (1) {
                      switch (_context.prev = _context.next) {
                        case 0:
                          conv = _value;
                          charIndex = _this.users.findIndex(function (val) {
                            return val === conv.username;
                          }) % _this.chars.length;
                          text = conv.text.replace(/@[^\s]*/g, '');
                          text = text.replace(urlRegex, '');
                          text = text.replace(/\s\s+/g, '');

                          _this.chars[charIndex].jumpUp();

                          _this.chars.forEach(function (char) {
                            return char.setDepth(_defaults.default.charDepth);
                          });

                          _this.chars[charIndex].setDepth(10);

                          _context.next = 10;
                          return _this.chars[charIndex].speak(text, 45);

                        case 10:
                          _this.chars[charIndex].voxbox.close();

                        case 11:
                        case "end":
                          return _context.stop();
                      }
                    }
                  }, _loop);
                });
                _iterator = (0, _asyncIterator2.default)(this.convo.reverse());

              case 5:
                _context2.next = 7;
                return _iterator.next();

              case 7:
                _step = _context2.sent;
                _iteratorNormalCompletion = _step.done;
                _context2.next = 11;
                return _step.value;

              case 11:
                _value = _context2.sent;

                if (_iteratorNormalCompletion) {
                  _context2.next = 17;
                  break;
                }

                return _context2.delegateYield(_loop(), "t0", 14);

              case 14:
                _iteratorNormalCompletion = true;
                _context2.next = 5;
                break;

              case 17:
                _context2.next = 23;
                break;

              case 19:
                _context2.prev = 19;
                _context2.t1 = _context2["catch"](2);
                _didIteratorError = true;
                _iteratorError = _context2.t1;

              case 23:
                _context2.prev = 23;
                _context2.prev = 24;

                if (!(!_iteratorNormalCompletion && _iterator.return != null)) {
                  _context2.next = 28;
                  break;
                }

                _context2.next = 28;
                return _iterator.return();

              case 28:
                _context2.prev = 28;

                if (!_didIteratorError) {
                  _context2.next = 31;
                  break;
                }

                throw _iteratorError;

              case 31:
                return _context2.finish(28);

              case 32:
                return _context2.finish(23);

              case 33:
                setTimeout(function () {
                  return _this.runConvo();
                }, 5000);

              case 34:
              case "end":
                return _context2.stop();
            }
          }
        }, _callee, this, [[2, 19, 23, 33], [24,, 28, 32]]);
      }));

      function runConvo() {
        return _runConvo.apply(this, arguments);
      }

      return runConvo;
    }()
  }]);
  return TweetConvo;
}();

exports.default = TweetConvo;
//# sourceMappingURL=TweetConvo.js.map