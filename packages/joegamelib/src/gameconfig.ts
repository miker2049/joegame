import 'phaser'
/**
 * This returns a full config you can pass into `new Phaser.Game(config)`
 * */
// TODO type for resolver
const config: Phaser.Types.Core.GameConfig = {
  type: Phaser.WEBGL,
  render: {
    pixelArt: true
  },
  backgroundColor: 'red',
  scale: {
    mode: Phaser.Scale.MAX_ZOOM,
    width: 800,
    height: 600
  },
  parent: 'frame',
  dom: {
    createContainer: false
  },
  physics: {
    default: 'arcade',
    arcade: {
      gravity: { y: 0 },
      debug: false
    }
  }
}
export default config
