import Character from '../Character'

export default function (gobject: Character) {
  gobject.scene.tweens.add({
    targets: [gobject],
    onStart: () => {
      // this.charBody.setVelocity(0,0);
      if (gobject instanceof Character) {
        gobject.body.setEnable(false)
      }
    },
    onComplete: () => {
      if (gobject instanceof Character) {
        gobject.body.setEnable(true)
      }
    },
    y: '-= 4',
    ease: 'Quad',
    duration: 100,
    yoyo: true
  })
}
