import { LevelConfig } from 'ILevelConfig'
import 'phaser'
import TiledRawJSON from 'types/TiledRawJson'
import { getMapKeyNameRaw } from './getKeyNames'

export default async function (game: Phaser.Game, config: LevelConfig) {
  const scene = game.scene.getScenes(false, false)[0]
  const path = config.mapPath
  const rawmap: TiledRawJSON =
    config.mapData || (await (await fetch(scene.load.baseURL + path)).json())

  await Promise.all(
    rawmap.tilesets.map((tileset, i) => {
      if (tileset.source) {
        const fixpath = new URL(
          tileset.source,
          location.origin + scene.load.baseURL + path
        ).toString()
        return fetch(fixpath)
          .then((res) => res.json())
          .then((tilejson) => {
            const fiximgpath = new URL(tilejson.image, fixpath).toString()
            tilejson.image = fiximgpath
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
  scene.cache.json.add(getMapKeyNameRaw(path), rawmap)
  return game
}
