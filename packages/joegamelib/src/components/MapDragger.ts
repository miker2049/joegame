import Phaser from 'phaser'
export function mapDragger(scene: Phaser.Scene, factor = 0.8) {
  const cam = scene.cameras.main
  scene.input.on('pointermove', function (p) {
    if (!p.isDown) return
    cam.scrollX -= ((p.x - p.prevPosition.x) / cam.zoom) * factor
    cam.scrollY -= ((p.y - p.prevPosition.y) / cam.zoom) * factor
  })
}
