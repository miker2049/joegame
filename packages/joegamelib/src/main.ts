/**
 * @module joegame
 */
import TiledRawJSON, { ILayer } from './types/TiledRawJson'
import { LevelScene } from './LevelScene'
import { unzlibSync } from 'fflate'

function parseCompressed(input: string): number[] {
  try {
    const d = Uint8Array.from(atob(input), (c) => c.charCodeAt(0))
    const result = unzlibSync(d)
    const arr = new Int32Array(result.buffer)
    const out = Array.from(arr)
    return out
  } catch (err) {
    throw Error('Error parsing compressed layers:  ' + err)
  }
}

function inflateLayers(layers: ILayer[]): ILayer[] {
  return layers.map((l) => {
    if (l.type === 'tilelayer' && typeof l.data === 'string') {
      return {
        height: l.height,
        width: l.width,
        id: l.id,
        name: l.name,
        visible: l.visible,
        opacity: l.opacity,
        properties: [],
        x: l.x,
        y: l.y,
        type: 'tilelayer',
        draworder: 'topdown',
        data: parseCompressed(l.data)
      }
    } else return l
  })
}

export async function loadLevel(
  map: TiledRawJSON,
  key: string,
  gameConfigOverride?: Phaser.Types.Core.GameConfig
) {
  // const embedded = await embedTilesets(map)
  // const saturated = objectsAndPack(embedded)
  map.layers = inflateLayers(map.layers)
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
