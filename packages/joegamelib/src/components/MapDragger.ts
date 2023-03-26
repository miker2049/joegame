import Phaser from 'phaser'
export function mapDragger(scene: Phaser.Scene, factor = 0.8) {
  const cam = scene.cameras.main
  scene.input.on('pointermove', function (p) {
    if (!p.isDown) return
    cam.scrollX -= ((p.x - p.prevPosition.x) / cam.zoom) * factor
    cam.scrollY -= ((p.y - p.prevPosition.y) / cam.zoom) * factor
  })

  scene.input.on(
    Phaser.Input.Events.POINTER_WHEEL,
    ({ deltaY }: { deltaY: number }) => {
      if (deltaY > 0) {
        const am = cam.zoom + 1.2
        cam.zoomTo(am > 10 ? 10 : am)
      } else {
        const am = cam.zoom - 1.2
        cam.zoomTo(am < 0.2 ? 0.2 : am)
      }
    }
  )
}
