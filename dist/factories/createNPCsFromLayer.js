"use strict";

var _regeneratorRuntime2 = require("@babel/runtime/regenerator");

var _interopRequireDefault = require("@babel/runtime/helpers/interopRequireDefault");

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.default = _callee;

var _regenerator = _interopRequireDefault(require("@babel/runtime/regenerator"));

require("phaser");

var _createNPC = _interopRequireDefault(require("../utils/createNPC"));

var _marked = /*#__PURE__*/_regeneratorRuntime2.mark(_callee);

function _createForOfIteratorHelper(o, allowArrayLike) { var it; if (typeof Symbol === "undefined" || o[Symbol.iterator] == null) { if (Array.isArray(o) || (it = _unsupportedIterableToArray(o)) || allowArrayLike && o && typeof o.length === "number") { if (it) o = it; var i = 0; var F = function F() {}; return { s: F, n: function n() { if (i >= o.length) return { done: true }; return { done: false, value: o[i++] }; }, e: function e(_e) { throw _e; }, f: F }; } throw new TypeError("Invalid attempt to iterate non-iterable instance.\nIn order to be iterable, non-array objects must have a [Symbol.iterator]() method."); } var normalCompletion = true, didErr = false, err; return { s: function s() { it = o[Symbol.iterator](); }, n: function n() { var step = it.next(); normalCompletion = step.done; return step; }, e: function e(_e2) { didErr = true; err = _e2; }, f: function f() { try { if (!normalCompletion && it.return != null) it.return(); } finally { if (didErr) throw err; } } }; }

function _unsupportedIterableToArray(o, minLen) { if (!o) return; if (typeof o === "string") return _arrayLikeToArray(o, minLen); var n = Object.prototype.toString.call(o).slice(8, -1); if (n === "Object" && o.constructor) n = o.constructor.name; if (n === "Map" || n === "Set") return Array.from(o); if (n === "Arguments" || /^(?:Ui|I)nt(?:8|16|32)(?:Clamped)?Array$/.test(n)) return _arrayLikeToArray(o, minLen); }

function _arrayLikeToArray(arr, len) { if (len == null || len > arr.length) len = arr.length; for (var i = 0, arr2 = new Array(len); i < len; i++) { arr2[i] = arr[i]; } return arr2; }

function _callee(layer, level, depth) {
  var interestSets, _iterator, _step, obj_, name;

  return _regenerator.default.wrap(function _callee$(_context) {
    while (1) {
      switch (_context.prev = _context.next) {
        case 0:
          if (level.map.getObjectLayer(layer)) {
            _context.next = 2;
            break;
          }

          return _context.abrupt("return");

        case 2:
          // first, create an object with keys that correspond to the npc names
          // that have values which are sets of locations in absolute pixels
          interestSets = {};
          _iterator = _createForOfIteratorHelper(level.map.getObjectLayer(layer).objects);

          try {
            for (_iterator.s(); !(_step = _iterator.n()).done;) {
              obj_ = _step.value;

              if (interestSets[obj_.name]) {
                interestSets[obj_.name].push({
                  x: obj_.x,
                  y: obj_.y,
                  NPCtype: obj_.type
                });
              } else {
                interestSets[obj_.name] = [{
                  x: obj_.x,
                  y: obj_.y,
                  NPCtype: obj_.type
                }];
              }
            } // now we iterate through our new object and create the npcs

          } catch (err) {
            _iterator.e(err);
          } finally {
            _iterator.f();
          }

          _context.t0 = _regenerator.default.keys(interestSets);

        case 6:
          if ((_context.t1 = _context.t0()).done) {
            _context.next = 13;
            break;
          }

          name = _context.t1.value;
          // create the npc
          console.log(interestSets);
          _context.next = 11;
          return (0, _createNPC.default)(name, interestSets[name], level);

        case 11:
          _context.next = 6;
          break;

        case 13:
          ;

        case 14:
        case "end":
          return _context.stop();
      }
    }
  }, _marked);
}