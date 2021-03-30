"use strict";

var _interopRequireDefault = require("@babel/runtime/helpers/interopRequireDefault");

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.moveToTile = moveToTile;

require("phaser");

var _joegameTypes = require("../joegameTypes");

var _ensure = _interopRequireDefault(require("../utils/ensure"));

function _createForOfIteratorHelper(o, allowArrayLike) { var it; if (typeof Symbol === "undefined" || o[Symbol.iterator] == null) { if (Array.isArray(o) || (it = _unsupportedIterableToArray(o)) || allowArrayLike && o && typeof o.length === "number") { if (it) o = it; var i = 0; var F = function F() {}; return { s: F, n: function n() { if (i >= o.length) return { done: true }; return { done: false, value: o[i++] }; }, e: function e(_e) { throw _e; }, f: F }; } throw new TypeError("Invalid attempt to iterate non-iterable instance.\nIn order to be iterable, non-array objects must have a [Symbol.iterator]() method."); } var normalCompletion = true, didErr = false, err; return { s: function s() { it = o[Symbol.iterator](); }, n: function n() { var step = it.next(); normalCompletion = step.done; return step; }, e: function e(_e2) { didErr = true; err = _e2; }, f: function f() { try { if (!normalCompletion && it.return != null) it.return(); } finally { if (didErr) throw err; } } }; }

function _unsupportedIterableToArray(o, minLen) { if (!o) return; if (typeof o === "string") return _arrayLikeToArray(o, minLen); var n = Object.prototype.toString.call(o).slice(8, -1); if (n === "Object" && o.constructor) n = o.constructor.name; if (n === "Map" || n === "Set") return Array.from(o); if (n === "Arguments" || /^(?:Ui|I)nt(?:8|16|32)(?:Clamped)?Array$/.test(n)) return _arrayLikeToArray(o, minLen); }

function _arrayLikeToArray(arr, len) { if (len == null || len > arr.length) len = arr.length; for (var i = 0, arr2 = new Array(len); i < len; i++) { arr2[i] = arr[i]; } return arr2; }

function moveOnPath(params) {
  // this.char.body.allowDrag = false;
  var tweens = [];

  var _iterator = _createForOfIteratorHelper(params.pathTransitions),
      _step;

  try {
    for (_iterator.s(); !(_step = _iterator.n()).done;) {
      var transition = _step.value;
      tweens.push(getTileMoveTween(transition, params.charControl, params.tileSize, params.speed));
    }
  } catch (err) {
    _iterator.e(err);
  } finally {
    _iterator.f();
  }

  tweens.push({
    targets: [params.charControl.charBody],
    duration: 100,
    onComplete: function onComplete() {
      if (params.finalFacing != undefined) {
        params.charControl.face(params.finalFacing);
      }

      params.charControl.stop();
      params.cb;
    }
  });
  return params.tweener.timeline({
    tweens: tweens
  });
}

function moveToTile(params) {
  var timeline = 'NO_PATH'; // destroy the current tween timeline that is running
  // this.char.destTimeline.destroy();
  //This janky movement doesnt seem to matter so much when you start moving and animating, makes sure the distance calculations are correct too

  params.charController.align(); // get the players current tile
  // here the callback first takes the path and makes it in to a bunch of (-1/0/1,-1/0/1) type transitions, then puts that into the path function

  if (params.tempObsX != undefined && params.tempObsY != undefined) {
    params.finder.avoidAdditionalPoint(params.tempObsX, params.tempObsY);
  }

  params.finder.findPath(params.x, params.y, params.dx, params.dy, function (path) {
    console.log(path);

    if (path) {
      var fpParams = {
        pathTransitions: createTransitions(path),
        tweener: params.tweener,
        charControl: params.charController,
        tileSize: params.tileWidth,
        speed: params.speed,
        cb: params.cb
      };
      timeline = moveOnPath(fpParams);
    } else {
      //TODO handle no path
      timeline = 'NO_PATH';
    }
  });
  params.finder.calculate();

  if (params.tempObsX != undefined && params.tempObsY != undefined) {
    params.finder.stopAvoidingAdditionalPoint(params.tempObsX, params.tempObsY);
  }

  return timeline;
}

function createTransitions(path) {
  var transitions = [];

  for (var i = 1; i < path.length; i++) {
    var xDiff = (0, _ensure.default)(path[i].x) - (0, _ensure.default)(path[i - 1].x);
    var yDiff = (0, _ensure.default)(path[i].y) - (0, _ensure.default)(path[i - 1].y);
    transitions.push({
      x: xDiff,
      y: yDiff
    });
  }

  return transitions;
}

function getTileMoveTween(pathTransition, char, tileSize, speed) {
  //accelration is the bodies change in velocity in pixels per second squared
  // velocity is pixels per second
  // we need to move exactly 8 pixels
  // ok but sometimes we are at top speed and sometimes not
  // 16
  console.log(tileSize / speed * 1000);
  return {
    targets: [],
    paused: false,
    onStart: function onStart(tween, targets, param) {
      var state = getStateFromTransition(pathTransition);
      console.log(state);
      char.move(_joegameTypes.Dir[state]);
    },
    duration: tileSize / speed * 1000,
    //have to disallow drag here for this
    ease: 'Stepped'
  };
} //TODO everything should use dir enum...


function getStateFromTransition(pathTransition) {
  if (pathTransition.x === 1 && pathTransition.y === 0) {
    return _joegameTypes.Dir.east; // return 'east';
  } else if (pathTransition.x === -1 && pathTransition.y === 0) {
    return _joegameTypes.Dir.west; // return 'west';
  } else if (pathTransition.x === 0 && pathTransition.y === 1) {
    return _joegameTypes.Dir.south; // return 'south';
  } else if (pathTransition.x === 0 && pathTransition.y === -1) {
    return _joegameTypes.Dir.north; // return 'north';
  } else {
    return _joegameTypes.Dir.north; // return 'north';
  }
}