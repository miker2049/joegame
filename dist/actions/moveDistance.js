"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.default = _default;

var _joegameTypes = require("../joegameTypes");

var THRESHOLD = 0.25;
var DELAY = 2;

function _default(params) {
  return new Promise(function (res, rej) {
    var axis;
    var dest;

    switch (params.dir) {
      case _joegameTypes.Dir.north:
        axis = _joegameTypes.Axis.yaxis;
        dest = params.gobject.y - params.distance;
        break;

      case _joegameTypes.Dir.south:
        axis = _joegameTypes.Axis.yaxis;
        dest = params.gobject.y + params.distance;
        break;

      case _joegameTypes.Dir.east:
        dest = params.gobject.x + params.distance;
        axis = _joegameTypes.Axis.xaxis;
        break;

      case _joegameTypes.Dir.west:
        dest = params.gobject.x - params.distance;
        axis = _joegameTypes.Axis.xaxis;
        break;
    }

    var timeRun = 0;
    params.gobject.move(params.dir);
    setTimeout(function () {
      res(undefined);
    }, params.distance / params.gobject.speed * 1000); // var inter = setInterval(() => {
    //     const target = axis === Axis.yaxis ? params.gobject.y : params.gobject.x
    //     if (Math.abs(dest - target) < THRESHOLD || target > dest) {
    //         clearInterval(inter)
    //         if (params.stop === true) params.gobject.stop()
    //         res(undefined)
    //     }
    //     if (((params.distance / params.gobject.speed) * 1000) / (timeRun * DELAY) < 1 / 2) {
    //         clearInterval(inter)
    //     }
    //     timeRun++
    // }, DELAY)
    // params.moveMachine.onTransition(state=> state.value != 'onPath' ? clearInterval(inter) : undefined )
  });
}