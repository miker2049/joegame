import { getSceneKeyName, getDialogueKeyName } from './utils/getKeyNames'
import { ICharacter } from './ICharacter'
import createTilemap from './factories/createTilemap'
import createPathfinder from './factories/createPathfinder'
import { IMachineRegistry, MachineRegistry } from './components/MachineRegistry'
import DialogueReader from './components/DialogueReader'
import { ILevelComponents, IPathfinder } from './ILevel'
// import Toner from './sound/Toner'

export class Level implements ILevelComponents {
    map: Phaser.Tilemaps.Tilemap
    player?: ICharacter
    pathfinder: IPathfinder
    npcs: Phaser.Physics.Arcade.Group
    platforms: Phaser.Physics.Arcade.Group
    scene: Phaser.Scene
    machineRegistry: IMachineRegistry
    // toner: Toner
    key: string

    constructor(game: Phaser.Game, mapjsonpath?: string) {
        this.key = mapjsonpath ? mapjsonpath : 'empty'
        this.scene = game.scene.add(getSceneKeyName(this.key), new Phaser.Scene(getSceneKeyName(this.key)), true)
        this.npcs = this.scene.physics.add.group()
        this.platforms = this.scene.physics.add.group()
        this.map = createTilemap(this.scene, this.key)
        this.pathfinder = createPathfinder(this.map)
        this.machineRegistry = new MachineRegistry()
        // this.toner = new Toner((game.sound as Phaser.Sound.WebAudioSoundManager).context);
    }
}
