"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.default = _default;

require("phaser");

function _default(char, x_, y_) {
  var residual = char.scene.add.image(x_ || char.x, y_ || char.y, char.texture.key, char.frame.name).setScale(char.scale).setAlpha(0.5).setDepth(10);
  char.scene.tweens.add({
    targets: [residual],
    alpha: 0,
    duration: 500,
    onComplete: function onComplete() {
      residual.destroy();
    }
  });
}
//# sourceMappingURL=createResidualGraphic.js.map