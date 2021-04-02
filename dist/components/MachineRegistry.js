"use strict";

var _interopRequireDefault = require("@babel/runtime/helpers/interopRequireDefault");

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.MachineRegistry = void 0;

var _classCallCheck2 = _interopRequireDefault(require("@babel/runtime/helpers/classCallCheck"));

var _createClass2 = _interopRequireDefault(require("@babel/runtime/helpers/createClass"));

var _xstate = require("xstate");

var MachineRegistry = /*#__PURE__*/function () {
  function MachineRegistry() {
    (0, _classCallCheck2.default)(this, MachineRegistry);
    this.machines = new Map();
  }

  (0, _createClass2.default)(MachineRegistry, [{
    key: "add",
    value: function add(char, mach) {
      this.machines.set(char, mach);
    }
  }, {
    key: "startAll",
    value: function startAll() {
      this.machines.forEach(function (mach) {
        return mach.start();
      });
    }
  }, {
    key: "stopAll",
    value: function stopAll() {
      this.machines.forEach(function (mach) {
        return mach.stop();
      });
    }
  }, {
    key: "sendTo",
    value: function sendTo(char, event) {
      var charm = this.machines.get(char);

      if (charm != undefined) {
        charm.send(event);
      } else {
        console.log("There is not ".concat(char, " machine in the registry"));
      }
    }
  }, {
    key: "checkStatus",
    value: function checkStatus(mach) {
      var _this$machines$get;

      return ((_this$machines$get = this.machines.get(mach)) === null || _this$machines$get === void 0 ? void 0 : _this$machines$get.status) || _xstate.InterpreterStatus.NotStarted;
    }
  }]);
  return MachineRegistry;
}();

exports.MachineRegistry = MachineRegistry;
//# sourceMappingURL=MachineRegistry.js.map