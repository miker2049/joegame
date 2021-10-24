import { getSceneKeyName } from './utils/getKeyNames'
import { ICharacter } from './ICharacter'
import createTilemap from './factories/createTilemap'
import createPathfinder from './factories/createPathfinder'
import { IMachineRegistry, MachineRegistry } from './components/MachineRegistry'
import { ILevelComponents, IPathfinder } from './ILevel'
import Toner from './sound/Toner'
import { ILevelConfig } from 'ILevelConfig'
import createPlayer from 'factories/createPlayer'

export class Level implements ILevelComponents {
    map: Phaser.Tilemaps.Tilemap
    player: ICharacter
    pathfinder: IPathfinder
    npcs: Phaser.Physics.Arcade.Group
    platforms: Phaser.Physics.Arcade.Group
    scene: Phaser.Scene
    config: ILevelConfig
    machineRegistry: IMachineRegistry
    toner: Toner
    key: string

    constructor(game: Phaser.Game, config: ILevelConfig) {
        this.config = config
        this.key = config.mapPath
        this.machineRegistry = new MachineRegistry()
        this.scene = game.scene.add(
            getSceneKeyName(this.key),
            new Phaser.Scene(getSceneKeyName(this.key)),
            true
        )

        this.scene.load.setBaseURL(this.scene.registry.get('loaderBaseURL'))
        this.npcs = this.scene.physics.add.group()
        this.platforms = this.scene.physics.add.group()
        this.map = createTilemap(this, this.key)
        this.pathfinder = createPathfinder(this.map)

        this.player = createPlayer(config.playerChar, config.playerStart.x,config.playerStart.y,this)
        this.scene.add.existing(this.player)
        this.player.sprite.setVisible(config.playerVisible)

        this.toner = new Toner(this);
    }
}
