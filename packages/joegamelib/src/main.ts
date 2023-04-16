/**
 * @module joegame
 */
import TiledRawJSON, { ILayer } from './types/TiledRawJson'
import { LevelScene } from './LevelScene'
import { inflateLayer } from './inflateLayer'

export async function loadLevel(
  map: TiledRawJSON,
  key: string,
  gameConfigOverride?: Phaser.Types.Core.GameConfig
) {
  // const embedded = await embedTilesets(map)
  // const saturated = objectsAndPack(embedded)
  map.layers = await Promise.all(map.layers.map((l) => inflateLayer(l)))
  const t = await Promise.all(map.layers.map((l) => inflateLayer(l)))
  console.log(t)
  const pack = JSON.parse(
    map.properties.find((p) => p.name === 'pack')?.value || '{}'
  )
  const _scene = new LevelScene(key, { ...map, pack })

  const defaultGameConfig: Phaser.Types.Core.GameConfig = {
    width: window.innerWidth,
    height: window.innerHeight,
    canvasStyle:
      'position: absolute; top: 0; left: 0; z-index: 1; background-color: #000000;',
    render: {
      pixelArt: true,
      transparent: true
    },
    scale: {
      mode: Phaser.Scale.RESIZE
    },
    physics: {
      default: 'arcade',
      arcade: {
        debug: true
      }
    }
  }

  const finalConfig = {
    ...defaultGameConfig,
    ...gameConfigOverride
  }
  await new Promise<Phaser.Game>((res) => {
    const _game = new Phaser.Game(finalConfig)
    _game.scene.add(key, _scene, true)
    _game.events.once('ready', () => res(_game))
  })
  const scene = await new Promise<LevelScene>((res) => {
    _scene.events.once('levelready', () => res(_scene))
  })
  return scene
}
