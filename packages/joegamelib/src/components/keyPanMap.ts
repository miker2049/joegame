import Phaser from 'phaser'

/*
 *  This component allows the user to pan the map with the arrow keys.
 */
export function keyPanMap(scene: Phaser.Scene, factor = 1) {
  const camera = scene.cameras.main
  scene.input.keyboard.on('keydown', (event: KeyboardEvent) => {
    if (event.key === 'ArrowUp') {
      camera.scrollY -= 10 * factor
    }
    if (event.key === 'ArrowDown') {
      camera.scrollY += 10 * factor
    }
    if (event.key === 'ArrowLeft') {
      camera.scrollX -= 10 * factor
    }
    if (event.key === 'ArrowRight') {
      camera.scrollX += 10 * factor
    }
  })
}
