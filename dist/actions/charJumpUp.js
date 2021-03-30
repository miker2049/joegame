"use strict";

var _interopRequireDefault = require("@babel/runtime/helpers/interopRequireDefault");

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.default = _default;

var _Character = _interopRequireDefault(require("../Character"));

function _default(gobject) {
  gobject.scene.tweens.add({
    targets: [gobject],
    onStart: function onStart() {
      // this.charBody.setVelocity(0,0);
      if (gobject instanceof _Character.default) {
        gobject.charBody.setEnable(false);
      }
    },
    onComplete: function onComplete() {
      if (gobject instanceof _Character.default) {
        gobject.charBody.setEnable(true);
      }
    },
    y: '-= 4',
    ease: 'Quad',
    duration: 100,
    yoyo: true
  });
}