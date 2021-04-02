"use strict";

var _interopRequireDefault = require("@babel/runtime/helpers/interopRequireDefault");

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.default = _default;

var _regenerator = _interopRequireDefault(require("@babel/runtime/regenerator"));

var _asyncToGenerator2 = _interopRequireDefault(require("@babel/runtime/helpers/asyncToGenerator"));

require("phaser");

function _default(_x, _x2, _x3) {
  return _ref.apply(this, arguments);
}

function _ref() {
  _ref = (0, _asyncToGenerator2.default)( /*#__PURE__*/_regenerator.default.mark(function _callee(scene, key, url) {
    return _regenerator.default.wrap(function _callee$(_context) {
      while (1) {
        switch (_context.prev = _context.next) {
          case 0:
            return _context.abrupt("return", new Promise(function (res, rej) {
              scene.load.json(key, url);

              if (scene.cache.json.exists(key)) {
                res(key);
              }

              scene.load.once('filecomplete', function (keyy) {
                // console.log(keyy)
                if (keyy === key) {
                  res(key);
                }
              });
              scene.load.once('loaderror', function (file) {
                if (file.key === key) {
                  rej(file);
                }
              });
              scene.load.start();
            }));

          case 1:
          case "end":
            return _context.stop();
        }
      }
    }, _callee);
  }));
  return _ref.apply(this, arguments);
}
//# sourceMappingURL=loadAfterLoad.js.map