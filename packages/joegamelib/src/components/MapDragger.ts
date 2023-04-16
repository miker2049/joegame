import Phaser from 'phaser'
import { LevelScene } from '../LevelScene'

export function mapDragger(scene: LevelScene, factor = 0.8) {
  const cam = scene.cameras.main
  scene.input.on('pointermove', function (p: Phaser.Input.Pointer) {
    if (!p.isDown) return
    cam.scrollX -= ((p.x - p.prevPosition.x) / cam.zoom) * factor
    cam.scrollY -= ((p.y - p.prevPosition.y) / cam.zoom) * factor
  })

  const map = scene.map
  const zoomFactor = 0.3
  scene.input.on(
    Phaser.Input.Events.POINTER_WHEEL,
    ({ deltaY }: { deltaY: number }) => {
      const edge = cam.getWorldPoint(cam.width, cam.height)
      if (deltaY > 0) {
        const am = cam.zoom + zoomFactor
        cam.zoomTo(am > 10 ? 10 : am)
      } else if (deltaY < 0) {
        const am = cam.zoom - zoomFactor
        cam.zoomTo(am < 2.5 ? 2.5 : am)
        if (edge.x <= map.widthInPixels) {
        }
      }
    }
  )
}
