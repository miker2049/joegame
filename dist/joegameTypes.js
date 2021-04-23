export var Dir;
(function (Dir) {
    Dir["north"] = "north";
    Dir["south"] = "south";
    Dir["east"] = "east";
    Dir["west"] = "west";
})(Dir || (Dir = {}));
export const VelocityMap = {
    north: [0, -1],
    south: [0, 1],
    east: [1, 0],
    west: [-1, 0]
};
export var Axis;
(function (Axis) {
    Axis[Axis["xaxis"] = 0] = "xaxis";
    Axis[Axis["yaxis"] = 1] = "yaxis";
})(Axis || (Axis = {}));
//# sourceMappingURL=joegameTypes.js.map