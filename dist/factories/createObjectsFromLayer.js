"use strict";

var _regeneratorRuntime2 = require("@babel/runtime/regenerator");

var _interopRequireDefault = require("@babel/runtime/helpers/interopRequireDefault");

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.default = _callee;

var _regenerator = _interopRequireDefault(require("@babel/runtime/regenerator"));

var _defineProperty2 = _interopRequireDefault(require("@babel/runtime/helpers/defineProperty"));

require("phaser");

var _MapObject = require("../components/MapObject");

var _marked = /*#__PURE__*/_regeneratorRuntime2.mark(_callee);

function ownKeys(object, enumerableOnly) { var keys = Object.keys(object); if (Object.getOwnPropertySymbols) { var symbols = Object.getOwnPropertySymbols(object); if (enumerableOnly) symbols = symbols.filter(function (sym) { return Object.getOwnPropertyDescriptor(object, sym).enumerable; }); keys.push.apply(keys, symbols); } return keys; }

function _objectSpread(target) { for (var i = 1; i < arguments.length; i++) { var source = arguments[i] != null ? arguments[i] : {}; if (i % 2) { ownKeys(Object(source), true).forEach(function (key) { (0, _defineProperty2.default)(target, key, source[key]); }); } else if (Object.getOwnPropertyDescriptors) { Object.defineProperties(target, Object.getOwnPropertyDescriptors(source)); } else { ownKeys(Object(source)).forEach(function (key) { Object.defineProperty(target, key, Object.getOwnPropertyDescriptor(source, key)); }); } } return target; }

function _createForOfIteratorHelper(o, allowArrayLike) { var it; if (typeof Symbol === "undefined" || o[Symbol.iterator] == null) { if (Array.isArray(o) || (it = _unsupportedIterableToArray(o)) || allowArrayLike && o && typeof o.length === "number") { if (it) o = it; var i = 0; var F = function F() {}; return { s: F, n: function n() { if (i >= o.length) return { done: true }; return { done: false, value: o[i++] }; }, e: function e(_e) { throw _e; }, f: F }; } throw new TypeError("Invalid attempt to iterate non-iterable instance.\nIn order to be iterable, non-array objects must have a [Symbol.iterator]() method."); } var normalCompletion = true, didErr = false, err; return { s: function s() { it = o[Symbol.iterator](); }, n: function n() { var step = it.next(); normalCompletion = step.done; return step; }, e: function e(_e2) { didErr = true; err = _e2; }, f: function f() { try { if (!normalCompletion && it.return != null) it.return(); } finally { if (didErr) throw err; } } }; }

function _unsupportedIterableToArray(o, minLen) { if (!o) return; if (typeof o === "string") return _arrayLikeToArray(o, minLen); var n = Object.prototype.toString.call(o).slice(8, -1); if (n === "Object" && o.constructor) n = o.constructor.name; if (n === "Map" || n === "Set") return Array.from(o); if (n === "Arguments" || /^(?:Ui|I)nt(?:8|16|32)(?:Clamped)?Array$/.test(n)) return _arrayLikeToArray(o, minLen); }

function _arrayLikeToArray(arr, len) { if (len == null || len > arr.length) len = arr.length; for (var i = 0, arr2 = new Array(len); i < len; i++) { arr2[i] = arr[i]; } return arr2; }

function _callee(tilemap, layer, depth, offsetX, offsetY) {
  var scene, _iterator, _step, obj, mobj;

  return _regenerator.default.wrap(function _callee$(_context) {
    while (1) {
      switch (_context.prev = _context.next) {
        case 0:
          if (tilemap.getObjectLayer(layer)) {
            _context.next = 2;
            break;
          }

          return _context.abrupt("return");

        case 2:
          scene = tilemap.scene;
          _iterator = _createForOfIteratorHelper(tilemap.getObjectLayer(layer).objects);
          _context.prev = 4;

          _iterator.s();

        case 6:
          if ((_step = _iterator.n()).done) {
            _context.next = 18;
            break;
          }

          obj = _step.value;
          mobj = _objectSpread({
            depth: depth
          }, obj);
          mobj.x = mobj.x ? mobj.x + (offsetX || 0) : 0;
          mobj.y = mobj.y ? mobj.y + (offsetY || 0) : 0;
          _context.t0 = mobj.type;
          _context.next = 14;
          break;

        case 14:
          _context.next = 16;
          return new _MapObject.MapObject(scene, tilemap, mobj.x, mobj.y, mobj);

        case 16:
          _context.next = 6;
          break;

        case 18:
          _context.next = 23;
          break;

        case 20:
          _context.prev = 20;
          _context.t1 = _context["catch"](4);

          _iterator.e(_context.t1);

        case 23:
          _context.prev = 23;

          _iterator.f();

          return _context.finish(23);

        case 26:
        case "end":
          return _context.stop();
      }
    }
  }, _marked, null, [[4, 20, 23, 26]]);
}