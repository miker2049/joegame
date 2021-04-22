"use strict";

var _interopRequireDefault = require("@babel/runtime/helpers/interopRequireDefault");

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.hashToArr = hashToArr;

var _toConsumableArray2 = _interopRequireDefault(require("@babel/runtime/helpers/toConsumableArray"));

//https://stackoverflow.com/a/7616484
function hashToArr(str, amt) {
  var hash = 0,
      i,
      chr;
  if (str.length === 0) return [hash];

  for (i = 0; i < str.length; i++) {
    chr = str.charCodeAt(i);
    hash = (hash << 7) - hash + chr;
    hash |= 0; // Convert to 32bit integer
  }

  hash = Math.abs(hash);
  var hashStr = hash.toString();
  hashStr = hashStr + hashStr; // assert(hash.toString().length >= amt, 'hash is big enough')
  // console.log(hash, "HASH length")

  return (0, _toConsumableArray2.default)(hashStr.slice(amt * -1)).map(function (item) {
    return Number(item);
  });
}

;
//# sourceMappingURL=hashToArr.js.map