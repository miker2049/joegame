/**
 * @module joegame
 */
import TiledRawJSON, { ILayer } from './types/TiledRawJson'
import { LevelScene } from './LevelScene'
import { TiledMap } from 'mapscripts/src/TiledMap'
import { unzlibSync } from 'fflate'

function parseCompressed(input: string): number[] {
  try {
    const d = Uint8Array.from(atob(input), (c) => c.charCodeAt(0))
    console.log(d)
    const result = unzlibSync(d)
    const arr = new Int32Array(result.buffer)
    const out = Array.from(arr)
    return out
  } catch (err) {
    throw Error(err)
  }
}
export class TiledMapInflated extends TiledMap {
  constructor(conf: TiledRawJSON) {
    super(conf)
    this.inflateLayers()
  }
  private inflateLayers() {
    const newLayers: ILayer[] = this.getConf().layers.map((l) => {
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
    this.updateConf({ layers: newLayers })
  }
}
export async function loadLevel(
  map: TiledRawJSON,
  key: string,
  gameConfigOverride?: Phaser.Types.Core.GameConfig
) {
  // const embedded = await embedTilesets(map)
  // const saturated = objectsAndPack(embedded)
  const inflated = new TiledMapInflated(map).getConf()
  const pack = JSON.parse(
    inflated.properties.find((p) => p.name === 'pack')?.value || '{}'
  )
  const _scene = new LevelScene(key, { ...inflated, pack })

  const defaultGameConfig = {
    render: {
      pixelArt: true,
      transparent: true
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
