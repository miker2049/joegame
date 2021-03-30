"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.default = createAnimsFromSheet;

require("phaser");

function createAnimsFromSheet(sheetKey, animLength, game) {
  var sheet = game.textures.get(sheetKey);

  for (var i = 0; i < sheet.frameTotal - 1; i = i + animLength) {
    // console.log(game.anims.generateFrameNumbers(sheetKey, { start: i, end: i + (animLength - 1), }))
    game.anims.create({
      key: "".concat(sheetKey, "_anim_").concat(i),
      frames: game.anims.generateFrameNumbers(sheetKey, {
        start: i,
        end: i + (animLength - 1)
      }),
      frameRate: 10,
      repeat: -1
    });
  }
}