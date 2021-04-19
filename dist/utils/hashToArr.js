"use strict";

var _interopRequireDefault = require("@babel/runtime/helpers/interopRequireDefault");

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.default = _default;

var _toConsumableArray2 = _interopRequireDefault(require("@babel/runtime/helpers/toConsumableArray"));

//https://stackoverflow.com/a/7616484
function _default(str, amt) {
  var hash = 0,
      i,
      chr;
  if (str.length === 0) return [hash];

  for (i = 0; i < str.length; i++) {
    chr = str.charCodeAt(i);
    hash = (hash << 5) - hash + chr;
    hash |= 0; // Convert to 32bit integer
  }

  return (0, _toConsumableArray2.default)(hash.toString().slice(amt * -1)).map(function (item) {
    return Number(item);
  });
}

;
//# sourceMappingURL=hashToArr.js.map