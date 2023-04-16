import Phaser from 'phaser'
import TiledRawJSON from './types/TiledRawJson'
import createTilemap from './factories/createTilemap'
import addAllObjectsFromLayer from './actions/addAllObjectsFromLayer'
import { PackType } from './types/custom'
import { mapDragger } from './components/MapDragger'
import { createPathfinder } from './factories/createPathfinder'
import { MachineRegistry } from './components/MachineRegistry'
import { keyPanMap } from './components/keyPanMap'

export class LevelScene extends Phaser.Scene {
  mapjson: TiledRawJSON & { pack: PackType }
  map: Phaser.Tilemaps.Tilemap
  pathfinder: ReturnType<typeof createPathfinder>
  machineRegistry: MachineRegistry
  constructor(key: string, map: TiledRawJSON & { pack: PackType }) {
    super(key)
    this.mapjson = map
    this.machineRegistry = new MachineRegistry()
  }
  preload() {
    this.load.addPack(this.mapjson.pack)
    this.load.tilemapTiledJSON('map', this.mapjson)
  }
  create() {
    this.map = createTilemap(this)
    this.pathfinder = createPathfinder(this.map)

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

    mapDragger(this)
    keyPanMap(this, 0.8)
    this.machineRegistry.startAll()
    this.events.emit('levelready')
    window.scene = this
  }
}
