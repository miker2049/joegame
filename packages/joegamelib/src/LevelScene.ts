import Phaser from 'phaser'
import TiledRawJSON from './types/TiledRawJson'
import createTilemap from './factories/createTilemap'
import addAllObjectsFromLayer from './actions/addAllObjectsFromLayer'

export type PackType = Record<
  string,
  { files: Array<{ type: string; key: string; url?: string }> }
>

export class LevelScene extends Phaser.Scene {
  mapjson: TiledRawJSON & { pack: PackType }
  map: Phaser.Tilemaps.Tilemap | undefined
  constructor(key: string, map: TiledRawJSON & { pack: PackType }) {
    super(key)
    this.mapjson = map
  }
  preload() {
    this.load.addPack(this.mapjson.pack)
    this.load.tilemapTiledJSON('map', this.mapjson)
  }
  create() {
    this.map = createTilemap(this)
    this.mapjson.layers.forEach((l) => {
      if (l.type === 'objectgroup') addAllObjectsFromLayer(this, l.name)
    })
    this.cameras.main.setZoom(4)
    this.cameras.main.setBounds(
      0,
      0,
      this.map.widthInPixels,
      this.map.heightInPixels
    )
    const cam = this.cameras.main
    const factor = 0.8
    this.input.on('pointermove', function (p) {
      if (!p.isDown) return
      cam.scrollX -= ((p.x - p.prevPosition.x) / cam.zoom) * factor
      cam.scrollY -= ((p.y - p.prevPosition.y) / cam.zoom) * factor
    })
  }
}
