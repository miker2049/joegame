"use strict";

var _interopRequireDefault = require("@babel/runtime/helpers/interopRequireDefault");

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.default = _default;

var _regenerator = _interopRequireDefault(require("@babel/runtime/regenerator"));

var _asyncToGenerator2 = _interopRequireDefault(require("@babel/runtime/helpers/asyncToGenerator"));

require("phaser");

var _createCineRunner = _interopRequireDefault(require("../factories/createCineRunner"));

var _createTextWindow = _interopRequireDefault(require("../factories/createTextWindow"));

var _getKeyNames = require("../utils/getKeyNames");

var _typewriteText = _interopRequireDefault(require("../utils/typewriteText"));

function _createForOfIteratorHelper(o, allowArrayLike) { var it; if (typeof Symbol === "undefined" || o[Symbol.iterator] == null) { if (Array.isArray(o) || (it = _unsupportedIterableToArray(o)) || allowArrayLike && o && typeof o.length === "number") { if (it) o = it; var i = 0; var F = function F() {}; return { s: F, n: function n() { if (i >= o.length) return { done: true }; return { done: false, value: o[i++] }; }, e: function e(_e) { throw _e; }, f: F }; } throw new TypeError("Invalid attempt to iterate non-iterable instance.\nIn order to be iterable, non-array objects must have a [Symbol.iterator]() method."); } var normalCompletion = true, didErr = false, err; return { s: function s() { it = o[Symbol.iterator](); }, n: function n() { var step = it.next(); normalCompletion = step.done; return step; }, e: function e(_e2) { didErr = true; err = _e2; }, f: function f() { try { if (!normalCompletion && it.return != null) it.return(); } finally { if (didErr) throw err; } } }; }

function _unsupportedIterableToArray(o, minLen) { if (!o) return; if (typeof o === "string") return _arrayLikeToArray(o, minLen); var n = Object.prototype.toString.call(o).slice(8, -1); if (n === "Object" && o.constructor) n = o.constructor.name; if (n === "Map" || n === "Set") return Array.from(o); if (n === "Arguments" || /^(?:Ui|I)nt(?:8|16|32)(?:Clamped)?Array$/.test(n)) return _arrayLikeToArray(o, minLen); }

function _arrayLikeToArray(arr, len) { if (len == null || len > arr.length) len = arr.length; for (var i = 0, arr2 = new Array(len); i < len; i++) { arr2[i] = arr[i]; } return arr2; }

function _default(_x, _x2) {
  return _ref.apply(this, arguments);
}

function _ref() {
  _ref = (0, _asyncToGenerator2.default)( /*#__PURE__*/_regenerator.default.mark(function _callee(level, node) {
    var yarnjson, textWindow, runner, _iterator, _step, result, command;

    return _regenerator.default.wrap(function _callee$(_context) {
      while (1) {
        switch (_context.prev = _context.next) {
          case 0:
            yarnjson = level.scene.cache.json.get((0, _getKeyNames.getDialogueKeyName)(level.key));
            textWindow = (0, _createTextWindow.default)({
              game: level.scene.game,
              x: 20,
              y: 20,
              width: level.scene.renderer.width * (2 / 3),
              height: level.scene.renderer.height * (1 / 3),
              additionalStyle: "padding-top: 1em"
            });
            runner = (0, _createCineRunner.default)(level, yarnjson, textWindow);
            _iterator = _createForOfIteratorHelper(runner.run(node));
            _context.prev = 4;

            _iterator.s();

          case 6:
            if ((_step = _iterator.n()).done) {
              _context.next = 26;
              break;
            }

            result = _step.value;
            _context.t0 = result.constructor.name;
            _context.next = _context.t0 === "TextResult" ? 11 : _context.t0 === "CommandResult" ? 17 : _context.t0 === "OptionsResult" ? 23 : 24;
            break;

          case 11:
            _context.next = 13;
            return (0, _typewriteText.default)(result.text, textWindow, level.scene, 50);

          case 13:
            textWindow.appendNewLineMDText('');
            _context.next = 16;
            return new Promise(function (resolve) {
              return setTimeout(resolve, 1000);
            });

          case 16:
            return _context.abrupt("break", 24);

          case 17:
            command = result;
            console.log(command);

            if (!(command.result instanceof Promise)) {
              _context.next = 22;
              break;
            }

            _context.next = 22;
            return command.result;

          case 22:
            return _context.abrupt("break", 24);

          case 23:
            return _context.abrupt("break", 24);

          case 24:
            _context.next = 6;
            break;

          case 26:
            _context.next = 31;
            break;

          case 28:
            _context.prev = 28;
            _context.t1 = _context["catch"](4);

            _iterator.e(_context.t1);

          case 31:
            _context.prev = 31;

            _iterator.f();

            return _context.finish(31);

          case 34:
          case "end":
            return _context.stop();
        }
      }
    }, _callee, null, [[4, 28, 31, 34]]);
  }));
  return _ref.apply(this, arguments);
}
//# sourceMappingURL=runCinematicNode.js.map