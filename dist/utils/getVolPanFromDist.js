"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.getVolAndPanFromDistance = getVolAndPanFromDistance;

function getVolAndPanFromDistance(playerX, playerY, charX, charY, cameraWidth) {
  var difference = charX - playerX; // const distance = Phaser.Math.Distance.BetweenPoints(difference ^ 2 + (charY - playerY) ^ 2)

  var distance = Phaser.Math.Distance.BetweenPoints({
    x: playerX,
    y: playerY
  }, {
    x: charX,
    y: charY
  });
  var pan = Phaser.Math.Clamp(difference / (cameraWidth / 2), -1, 1);
  var vol = Phaser.Math.Clamp(-(1 / (cameraWidth / 2)) * distance + 1, 0, 1);
  return [vol, pan];
}
//# sourceMappingURL=getVolPanFromDist.js.map