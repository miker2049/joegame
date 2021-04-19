"use strict";

var _interopRequireDefault = require("@babel/runtime/helpers/interopRequireDefault");

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.default = _default;

var _regenerator = _interopRequireDefault(require("@babel/runtime/regenerator"));

var _asyncToGenerator2 = _interopRequireDefault(require("@babel/runtime/helpers/asyncToGenerator"));

var _asyncIterator2 = _interopRequireDefault(require("@babel/runtime/helpers/asyncIterator"));

var _createTweetConvo = _interopRequireDefault(require("../factories/createTweetConvo"));

var _shuffleArr = _interopRequireDefault(require("../utils/shuffleArr"));

function _default(_x, _x2) {
  return _ref.apply(this, arguments);
}

function _ref() {
  _ref = (0, _asyncToGenerator2.default)( /*#__PURE__*/_regenerator.default.mark(function _callee(level, layer) {
    var convos, mani, _iteratorNormalCompletion, _didIteratorError, _iteratorError, _iterator, _step, _value, _obj_$properties$find, _obj_$properties, _obj_$properties$find2, _obj_$x, _obj_$y, obj_, convoIDD, charGroup;

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
            convos = [];
            mani = JSON.parse(JSON.stringify(level.scene.cache.json.get('convo-manifest')));
            _iteratorNormalCompletion = true;
            _didIteratorError = false;
            _context.prev = 6;
            _iterator = (0, _asyncIterator2.default)(level.map.getObjectLayer(layer).objects);

          case 8:
            _context.next = 10;
            return _iterator.next();

          case 10:
            _step = _context.sent;
            _iteratorNormalCompletion = _step.done;
            _context.next = 14;
            return _step.value;

          case 14:
            _value = _context.sent;

            if (_iteratorNormalCompletion) {
              _context.next = 28;
              break;
            }

            obj_ = _value;
            convoIDD = void 0; // const coord = level.map.tileToWorldXY(obj_.x, obj_.y)

            charGroup = (_obj_$properties$find = (_obj_$properties = obj_.properties) === null || _obj_$properties === void 0 ? void 0 : (_obj_$properties$find2 = _obj_$properties.find(function (prop) {
              return prop.name === 'charGroup';
            })) === null || _obj_$properties$find2 === void 0 ? void 0 : _obj_$properties$find2.value) !== null && _obj_$properties$find !== void 0 ? _obj_$properties$find : 'all';

            if (mani.length > 0) {
              convoIDD = (0, _shuffleArr.default)(mani).pop();
            } else {
              mani = JSON.parse(JSON.stringify(level.scene.cache.json.get('convo-manifest')));
              mani = mani.files;
              convoIDD = (0, _shuffleArr.default)(mani).pop();
            }

            _context.t0 = convos;
            _context.next = 23;
            return (0, _createTweetConvo.default)(level, (_obj_$x = obj_.x) !== null && _obj_$x !== void 0 ? _obj_$x : 0, (_obj_$y = obj_.y) !== null && _obj_$y !== void 0 ? _obj_$y : 0, charGroup, convoIDD);

          case 23:
            _context.t1 = _context.sent;

            _context.t0.push.call(_context.t0, _context.t1);

          case 25:
            _iteratorNormalCompletion = true;
            _context.next = 8;
            break;

          case 28:
            _context.next = 34;
            break;

          case 30:
            _context.prev = 30;
            _context.t2 = _context["catch"](6);
            _didIteratorError = true;
            _iteratorError = _context.t2;

          case 34:
            _context.prev = 34;
            _context.prev = 35;

            if (!(!_iteratorNormalCompletion && _iterator.return != null)) {
              _context.next = 39;
              break;
            }

            _context.next = 39;
            return _iterator.return();

          case 39:
            _context.prev = 39;

            if (!_didIteratorError) {
              _context.next = 42;
              break;
            }

            throw _iteratorError;

          case 42:
            return _context.finish(39);

          case 43:
            return _context.finish(34);

          case 44:
            return _context.abrupt("return", convos);

          case 45:
          case "end":
            return _context.stop();
        }
      }
    }, _callee, null, [[6, 30, 34, 44], [35,, 39, 43]]);
  }));
  return _ref.apply(this, arguments);
}
//# sourceMappingURL=addAllTweetConvosFromLayer.js.map