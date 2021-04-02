"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.default = _default;

function _default(n, x) {
  if (x > n) {
    return x;
  }

  n = n + x / 2;
  n = n - n % x;
  return n;
}
//# sourceMappingURL=closestMultiple.js.map