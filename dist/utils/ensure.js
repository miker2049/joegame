"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.default = _default;

// https://stackoverflow.com/a/54738437
function _default(argument) {
  var message = arguments.length > 1 && arguments[1] !== undefined ? arguments[1] : 'This value was promised to be there.';

  if (argument === undefined || argument === null) {
    throw new TypeError(message);
  }

  return argument;
}
//# sourceMappingURL=ensure.js.map