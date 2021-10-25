import 'phaser';
import closestMultiple from '../utils/closestMultiple';
import defaults from '../defaults';
import { IMap, ILevelComponents } from '../ILevel';
import { wikiPlatformEntry } from '../utils/parseWikiData';



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



    constructor(config: PlatformConfig) {

        super(config.level.scene, config.x * config.level.map.tileWidth, config.y * config.level.map.tileHeight)


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
            tiles.forEach(ti=>ti.setPipeline('Light2D'));
        }
        this.setDepth(config.depth)
        this.add(tiles);
        this.body = new Phaser.Physics.Arcade.Body(this.scene.physics.world, this)
        this.scene.physics.world.enable(this)
        this.body.setOffset((config.width * config.level.map.tileWidth) / 2, (config.height * config.level.map.tileHeight) / 2)
        this.runPlatform(config)
    }

    runPlatform(config: PlatformConfig) {
        let distance = Phaser.Math.Distance.Between(config.x, config.y, config.endX || config.x, config.endY || config.y) * this.tileSize
        const delay = (config.speed || 1) * 1000;
        const speed = Math.floor(distance / (delay / 1000));

        this.velX = Math.abs((config.endX || config.x) - config.x) > 1 ? (config.endX || config.x) > config.x ? speed : -speed : 0;
        this.velY = Math.abs((config.endY || config.y) - config.y) > 1 ? (config.endY || config.y) > config.y ? speed : -speed : 0;
        this.velX = this.velX * -1;
        this.velY = this.velY * -1;

        this.atHome = false;
        this.body.setMaxSpeed(speed)
        const cb = () => {
            const newVelX = this.velX
            const newVelY = this.velY
            // const accelX = this.velX != 0 ? this.velX > 0 ?  defaults.globalDrag : -defaults.globalDrag : 0
            // const accelY = this.velY != 0 ? this.velY > 0 ?  defaults.globalDrag : -defaults.globalDrag : 0
            // this.body.setAcceleration(accelX,accelY)
            this.body.setVelocity(newVelX, newVelY)
            this.notifyVelChange()
        }

        const mainCb = () => {
            this.atHome = !!!this.atHome;
            const snapX = this.atHome ? config.x : config.endX || config.x
            const snapY = this.atHome ? config.y : config.endY || config.y
            // console.log(this.atHome)
            this.body.setAcceleration(0, 0)
            this.body.setVelocity(0, 0)

            this.velX = this.velX * -1;
            this.velY = this.velY * -1;

            this.body.x = this.x = closestMultiple(snapX * this.tileSize, this.tileSize)
            this.body.y = this.y = closestMultiple(snapY * this.tileSize, this.tileSize)
            this.notifyVelChange()
            this.scene.time.addEvent({
                callback: cb,
                delay: this.pause
            })
        }
        mainCb()
        this.scene.time.addEvent({
            callback: mainCb,
            delay: delay + this.pause,
            loop: true,
        })
    }
    notifyVelChange() {
        //todo, we need a all player group or something, for when we want npcs on platforms
        // let test= this.scene.physics.overlap(this, this.scene.player, (plat, player)=>{
        //     const char = player as Character;
        //     char.moveMachine.send('PLATFORM_CHANGE', {vel: {x: plat.body.velocity.x, y: plat.body.velocity.y}})
        // })
        // this.scene.physics.overlap(this, this.scene.map.objbody,(plat,obj)=>{
        //     obj.moveMachine.send('PLATFORM_CHANGE', {vel: {x: plat.body.velocity.x, y: plat.body.velocity.y}})
        this.level.scene.physics.overlap(this.level.platforms, this.level.player, (player, plat) => {
            this.level.machineRegistry.sendTo("player_machine", { type: 'PLATFORM_CHANGE', vel: { x: plat.body.velocity.x, y: plat.body.velocity.y } })
        })
    }

}
