"use strict";

var _interopRequireDefault = require("@babel/runtime/helpers/interopRequireDefault");

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.default = _default;

var _createPathTransitions = _interopRequireDefault(require("./createPathTransitions"));

function _default(params) {
  return new Promise(function (res, rej) {
    if (params.tempObsX != undefined && params.tempObsY != undefined) {
      params.finder.avoidAdditionalPoint(params.tempObsX, params.tempObsY);
    } // console.log(params.x,params.y,params.dx,params.dy)


    params.finder.findPath(params.x, params.y, params.dx, params.dy, function (path) {
      if (path) {
        res((0, _createPathTransitions.default)(path));
      } else {
        rej();
      }
    });
    params.finder.calculate();

    if (params.tempObsX != undefined && params.tempObsY != undefined) {
      params.finder.stopAvoidingAdditionalPoint(params.tempObsX, params.tempObsY);
    }
  });
}