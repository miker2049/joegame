"use strict";

var _interopRequireDefault = require("@babel/runtime/helpers/interopRequireDefault");

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.default = _default;

var _regenerator = _interopRequireDefault(require("@babel/runtime/regenerator"));

var _asyncToGenerator2 = _interopRequireDefault(require("@babel/runtime/helpers/asyncToGenerator"));

require("phaser");

var _bondage = require("bondage");

function _default(level, yarnjson, textWindow) {
  var runner = new _bondage.Runner();
  runner.load(yarnjson);
  runner.setVariableStorage(level.scene.registry); //NOTE assuming square tile

  var tileSize = level.map.tileWidth;
  runner.registerFunction('wait', /*#__PURE__*/function () {
    var _ref = (0, _asyncToGenerator2.default)( /*#__PURE__*/_regenerator.default.mark(function _callee(args) {
      return _regenerator.default.wrap(function _callee$(_context) {
        while (1) {
          switch (_context.prev = _context.next) {
            case 0:
              _context.next = 2;
              return new Promise(function (resolve) {
                setTimeout(resolve, args[0]);
              });

            case 2:
            case "end":
              return _context.stop();
          }
        }
      }, _callee);
    }));

    return function (_x) {
      return _ref.apply(this, arguments);
    };
  }());
  runner.registerFunction('moveChar', function (args) {
    level.machineRegisty.sendTo(args[0], {
      type: 'MOVE_ON_PATH',
      point: {
        x: args[1] * tileSize,
        y: args[2] * tileSize
      }
    });
  });
  runner.registerFunction('moveCharSync', function (args) {
    level.machineRegisty.sendTo(args[0], {
      type: 'MOVE_ON_PATH',
      point: {
        x: args[1] * tileSize,
        y: args[2] * tileSize
      }
    });
    return new Promise(function (resolve) {
      level.machineRegisty.machines.get(args[0]).onTransition(function (state) {
        if (state.value === 'still') {
          resolve(null);
        }
      });
    });
  });
  runner.registerFunction('transportChar', function (args) {
    level.machineRegisty.sendTo(args[0], {
      type: 'TRANSPORT',
      point: {
        x: args[1] * tileSize,
        y: args[2] * tileSize
      }
    });
  });
  runner.registerFunction('openWindow', function (_args) {
    textWindow.open();
  });
  runner.registerFunction('closeWindow', function (_args) {
    textWindow.close();
  });
  runner.registerFunction('clearWindowText', function (_args) {
    textWindow.setMDText('');
  });
  return runner;
}