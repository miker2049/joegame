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

var _bondage = require("bondage");

var DialogueReader = /*#__PURE__*/function () {
  function DialogueReader(scene, yarnjson, registry, tilesize) {
    (0, _classCallCheck2.default)(this, DialogueReader);
    this.registry = registry;
    this.mapTileSize = tilesize;
    this.runner = new _bondage.Runner();
    this.runner.load(yarnjson);
    this.runner.setVariableStorage(scene.registry);
    this.allNodes = [];

    for (var node in this.runner.yarnNodes) {
      this.allNodes.push(this.runner.yarnNodes[node].title);
    }

    this.registerCommands();
  }

  (0, _createClass2.default)(DialogueReader, [{
    key: "registerCommands",
    value: function registerCommands() {
      var _this = this;

      this.runner.registerFunction('wait', /*#__PURE__*/function () {
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
      this.runner.registerFunction('moveChar', function (args) {
        console.log(args[0], {
          type: 'MOVE_ON_PATH',
          point: {
            x: args[1] * _this.mapTileSize,
            y: args[2] * _this.mapTileSize
          }
        });

        _this.registry.sendTo(args[0], {
          type: 'MOVE_ON_PATH',
          point: {
            x: args[1] * _this.mapTileSize,
            y: args[2] * _this.mapTileSize
          }
        });
      });
      this.runner.registerFunction('moveCharSync', function (args) {
        console.log(args[0], {
          type: 'MOVE_ON_PATH',
          point: {
            x: args[1] * _this.mapTileSize,
            y: args[2] * _this.mapTileSize
          }
        });

        _this.registry.sendTo(args[0], {
          type: 'MOVE_ON_PATH',
          point: {
            x: args[1] * _this.mapTileSize,
            y: args[2] * _this.mapTileSize
          }
        });

        return new Promise(function (resolve) {
          _this.registry.machines.get(args[0]).onTransition(function (state) {
            console.log(state);

            if (state.value === 'still') {
              resolve();
            }
          });
        });
      });
      this.runner.registerFunction('transportChar', function (args) {
        _this.registry.sendTo(args[0], {
          type: 'TRANSPORT',
          point: {
            x: args[1] * _this.mapTileSize,
            y: args[2] * _this.mapTileSize
          }
        });
      });
    }
  }, {
    key: "getRunner",
    value: function getRunner(node) {
      return this.runner.run(node);
    }
  }]);
  return DialogueReader;
}();

exports.default = DialogueReader;