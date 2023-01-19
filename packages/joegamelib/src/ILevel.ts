import 'phaser'
import { ICharacter } from './ICharacter'
import { IMachineRegistry } from './components/MachineRegistry'
import { BondageResults } from 'dialogue'
import { IToner } from './sound/IToner'
import { LevelConfig } from 'LevelConfig'

export interface IMapData {
  tileWidth: number
  tileHeight: number
  height: number
  width: number
  tilesets: Phaser.Tilemaps.Tileset[]
}

export interface IMapMethods {
  getObjectLayer(layer: string): Phaser.Tilemaps.ObjectLayer
  getLayer(layer: string): Phaser.Tilemaps.LayerData
  getTileAt(
    x: number,
    y: number,
    nonNull: boolean,
    layer: string
  ): { index: number; properties?: { collides?: boolean } }
}

//TODO what are we doing here?
export interface IMap extends IMapData, Phaser.Tilemaps.Tilemap {}

export interface DialogueRunner {
  getRunner(node: string): Generator<BondageResults, BondageResults, undefined>
}
export interface ILevelComponents {
  map: IMap
  player?: ICharacter
  pathfinder: IPathfinder
  config: LevelConfig
  npcs: Phaser.Physics.Arcade.Group
  platforms: Phaser.Physics.Arcade.Group
  scene: Phaser.Scene
  machineRegistry: IMachineRegistry
  toner?: IToner
  key: string
}
//
export interface IPathfinder {
  avoidAdditionalPoint(x: number, y: number): void
  stopAvoidingAdditionalPoint(x: number, y: number): void
  findPath(
    x: number,
    y: number,
    dx: number,
    dy: number,
    cb: (path: any) => void
  ): number
  calculate(): void
}
