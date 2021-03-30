"use strict";

var _interopRequireDefault = require("@babel/runtime/helpers/interopRequireDefault");

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.default = void 0;

require("phaser");

var _RainfallPostFX = _interopRequireDefault(require("./RainfallPostFX"));

var _WaterDropPostFX = require("./WaterDropPostFX");

var _DoomWipePostFX = require("./DoomWipePostFX");

var _PageCurlPostFX = require("./PageCurlPostFX");

var _WipePostFX = require("./WipePostFX");

var _PlasmaPostFX = _interopRequireDefault(require("./PlasmaPostFX"));

var _Blobs = _interopRequireDefault(require("./Blobs"));

var shaders = {
  RainfallPostFX: _RainfallPostFX.default,
  WaterDropPostFX: _WaterDropPostFX.WaterDropPostFX,
  DoomWipePostFX: _DoomWipePostFX.DoomWipePostFX,
  PageCurlPostFX: _PageCurlPostFX.PageCurlPostFX,
  WipePostFX: _WipePostFX.WipePostFX,
  PlasmaPostFX: _PlasmaPostFX.default,
  Blobs: _Blobs.default
};
var _default = shaders;
exports.default = _default;