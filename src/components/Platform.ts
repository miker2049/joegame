import 'phaser';
import closestMultiple from '../utils/closestMultiple';
import defaults from '../defaults';
import { IMap, ILevelComponents } from '../ILevel';
import { wikiPlatformEntry } from '../utils/parseWikiData';
import { createPlatformMachine } from './PlatformMachine';
import { interpret, Interpreter, StateMachine } from 'xstate';
import OverlapArea from './OverlapArea';
import ILevelFacade from 'ILevelFacade';
import { Overlapper } from './Overlapper';



interface PlatformConfig {
    level: ILevelComponents
    x: number
    y: number
    width: number
    height: number
    endX?: number
    endY?: number
    speed: number
    name: string
    depth: number
    ptype?: string
    auto: boolean
}

export default class Platform extends Phaser.GameObjects.Container {
    speed: number = 5
    acceleration: number = 100
    pause: number = 2000
    velX: number = 0
    velY: number = 0
    atHome: boolean = true
    tileSize: number = 16
    body: Phaser.Physics.Arcade.Body
    level: ILevelComponents
    machine: Interpreter<any, any, any>
    auto: boolean



    constructor(config: PlatformConfig) {

        super(config.level.scene, config.x, config.y)

        this.auto = config.auto

        this.level = config.level

        this.setName(config.name || `${config.x}x${config.y}x${config.endX}x${config.endY}`)
        //TODO make this dynamic
        const ptype = this.scene.cache.json.get('gdata').platform.get(config.ptype || 'default') as wikiPlatformEntry
        this.tileSize = config.level.map.tileWidth
        let tiles: Phaser.GameObjects.Image[] = [];
        for (let i = 0; i < config.width; i++) {
            for (let j = 0; j < config.height; j++) {
                let groundVersion = ptype.groundTiles[Math.floor(Math.random() * ptype.groundTiles.length)]
                tiles.push(this.scene.add.image(i * config.level.map.tileWidth, j * config.level.map.tileHeight, ptype.texture, groundVersion).setOrigin(0, 0).setDepth(config.depth))
                if (j === config.height - 1) {
                    tiles.push(this.scene.add.image(i * config.level.map.tileWidth, (j * config.level.map.tileHeight) + config.level.map.tileHeight, ptype.texture, ptype.edgeTiles[0])
                        .setOrigin(0, 0)
                        .setDepth(config.depth - 1))
                }
            }
        }

        this.setSize(config.width * config.level.map.tileWidth, config.height * config.level.map.tileHeight)
        if (this.level.config.lights) {
            tiles.forEach(ti => ti.setPipeline('Light2D'));
        }
        this.setDepth(config.depth)
        this.add(tiles);

        new Overlapper({
            scene: this.scene,

            enterCB: () => {
                console.log("entering")
                this.machine.send('PRESSED')
            },
            exitCB: () => {
                // console.log("leaving")
                // this.emit(config.emit)
            },
            active: true,
            checker: ()=>this.scene.physics.world.overlap(this,this.level.player)
        })

        this.body = new Phaser.Physics.Arcade.Body(this.scene.physics.world, this)
        this.scene.physics.world.enable(this)
        this.body.setOffset((config.width * config.level.map.tileWidth) / 2, (config.height * config.level.map.tileHeight) / 2)
        this.machine = interpret(createPlatformMachine({
            locations: [
                { x: config.x, y: config.y },
                { x: config.endX ?? config.x, y: (config.endY ?? config.y) }
            ],
            speed: 25,
            delay: this.pause,
            thisPlatform: this,
            auto: this.auto,
            currDistance: 0,
            currIndex: 0
        }))
        this.machine.start()
    }

    notifyVelChange() {
        this.level.scene.physics.overlap(this.level.platforms, this.level.player, (player, plat) => {
            this.level.machineRegistry.sendTo("player_machine", { type: 'PLATFORM_CHANGE', vel: { x: plat.body.velocity.x, y: plat.body.velocity.y } })
        })
    }

}
