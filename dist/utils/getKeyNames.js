"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.getDialogueKeyName = getDialogueKeyName;
exports.getMapKeyName = getMapKeyName;
exports.getMapKeyNameRaw = getMapKeyNameRaw;
exports.getKeyName = getKeyName;
exports.getSceneKeyName = getSceneKeyName;

function getKeyName(path) {
  return path.match(/[\w-]+\./)[0].replace(/\./, "");
}

function getSceneKeyName(path) {
  return getKeyName(path) + "_scene";
}

function getDialogueKeyName(path) {
  return getKeyName(path) + "_dialogue";
}

function getMapKeyName(path) {
  return getKeyName(path) + "_map";
}

function getMapKeyNameRaw(path) {
  return getMapKeyName(path) + "_raw";
}
//# sourceMappingURL=getKeyNames.js.map