"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.Axis = exports.VelocityMap = exports.Dir = void 0;
var Dir;
exports.Dir = Dir;

(function (Dir) {
  Dir["north"] = "north";
  Dir["south"] = "south";
  Dir["east"] = "east";
  Dir["west"] = "west";
})(Dir || (exports.Dir = Dir = {}));

var VelocityMap = {
  north: [0, -1],
  south: [0, 1],
  east: [1, 0],
  west: [-1, 0]
};
exports.VelocityMap = VelocityMap;
var Axis;
exports.Axis = Axis;

(function (Axis) {
  Axis[Axis["xaxis"] = 0] = "xaxis";
  Axis[Axis["yaxis"] = 1] = "yaxis";
})(Axis || (exports.Axis = Axis = {}));