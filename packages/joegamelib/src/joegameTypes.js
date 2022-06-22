"use strict";
exports.__esModule = true;
exports.Axis = exports.VelocityMap = exports.Dir = void 0;
// @ts-nocheck
var Dir;
(function (Dir) {
    Dir["north"] = "north";
    Dir["south"] = "south";
    Dir["east"] = "east";
    Dir["west"] = "west";
})(Dir = exports.Dir || (exports.Dir = {}));
exports.VelocityMap = {
    north: [0, -1],
    south: [0, 1],
    east: [1, 0],
    west: [-1, 0]
};
var Axis;
(function (Axis) {
    Axis[Axis["xaxis"] = 0] = "xaxis";
    Axis[Axis["yaxis"] = 1] = "yaxis";
})(Axis = exports.Axis || (exports.Axis = {}));
