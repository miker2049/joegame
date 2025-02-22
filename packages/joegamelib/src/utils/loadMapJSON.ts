import { LevelConfig } from '../LevelConfig'
import 'phaser'
import TiledRawJSON from '../types/TiledRawJson'
import { getMapKeyNameRaw } from './getKeyNames'

export default async function (game: Phaser.Game, config: LevelConfig) {
  const scene = game.scene.getScenes(false, false)[0]
  const path = config.mapPath
  const rawmap: TiledRawJSON =
    config.mapData || (await (await fetch(scene.load.baseURL + path)).json())
  await embedTilesets(rawmap)
  scene.cache.json.add(getMapKeyNameRaw(path), rawmap)
  return game
}

function fixPath(s: string, folder = 'images') {
  const stripped = s.split('/').reverse()[0]
  // @ts-ignore
  const basepath = BASEURL || '/'
  return `${basepath.replace(/\//g, '')}/assets/${folder.replace(
    /\//g,
    ''
  )}/${stripped}`
}

export async function embedTilesets(map: TiledRawJSON): Promise<TiledRawJSON> {
  let rawmap: TiledRawJSON = Object.assign({}, map)
  await Promise.all(
    rawmap.tilesets.map((tileset, i) => {
      if (tileset.source) {
        return fetch(fixPath(tileset.source))
          .then((res) => res.json())
          .then((tilejson) => {
            rawmap.tilesets[i] = {
              firstgid: rawmap.tilesets[i].firstgid,
              ...tilejson
            }
          })
      } else {
        return undefined
      }
    })
  )
  return rawmap
}
