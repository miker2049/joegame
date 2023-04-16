import { getSceneKeyName } from './utils/getKeyNames'
import { ICharacter } from './ICharacter'
import createTilemap from './factories/createTilemap'
import { createPathfinder } from './factories/createPathfinder'
import { IMachineRegistry, MachineRegistry } from './components/MachineRegistry'
import { ILevelComponents, IPathfinder } from './ILevel'
// import Toner from './sound/Toner'
import { LevelConfig } from './LevelConfig'
import createPlayer from './factories/createPlayer'
import defaults from './defaults'

export class Level implements ILevelComponents {
  map: Phaser.Tilemaps.Tilemap
  player?: ICharacter
  pathfinder: IPathfinder
  npcs: Phaser.Physics.Arcade.Group
  platforms: Phaser.Physics.Arcade.Group
  scene: Phaser.Scene
  config: LevelConfig
  machineRegistry: IMachineRegistry
  // toner: Toner
  key: string

  constructor(game: Phaser.Game, config: LevelConfig) {
    this.config = config
    this.key = config.mapPath
    this.machineRegistry = new MachineRegistry()
    this.scene = game.scene.add(
      getSceneKeyName(this.key),
      new Phaser.Scene(getSceneKeyName(this.key)),
      true
    )

    // console.log(this.scene.registry.get('loaderBaseURL'))
    // this.scene.load.setBaseURL(this.scene.registry.get('loaderBaseURL'))
    this.npcs = this.scene.physics.add.group()
    this.platforms = this.scene.physics.add.group()
    this.map = createTilemap(this, this.key)

    this.pathfinder = createPathfinder(this.map)
    if (!config.noPlayer) {
      const playerDepth =
        this.scene.game.registry.get('depthmap').get('Player') ??
        defaults.charDepth
      this.player = createPlayer(
        config.playerChar || 'player',
        config.playerStart?.x || this.map.widthInPixels / 2,
        config.playerStart?.y || this.map.heightInPixels / 2,
        this,
        playerDepth
      )
      this.scene.add.existing(this.player)
      this.player.sprite.setVisible(true)
    }
    //  this.toner = new Toner(this)
  }
}
