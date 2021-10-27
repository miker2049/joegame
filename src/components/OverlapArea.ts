// import 'phaser';
import { ILevelComponents } from 'ILevel';
import { MapObject } from './MapObject';
// import Character from './Character';
// import SceneMap from './SceneMap';
type Collider = Phaser.Types.Physics.Arcade.ArcadeColliderType
interface OverlapConfig {
    x: number
    y: number
    width: number
    height: number
    emit: string
    level: ILevelComponents
}
export default class OverlapArea extends MapObject {
    overlapped: boolean
    overlaptmp: boolean
    level: ILevelComponents
    deltabuff: number
    enterCallback: () => void
    leaveCallback: () => void

    constructor(config: OverlapConfig) {
        super({
            name: `${config.x}${config.y}${config.height}${config.width}`,
            id: config.x + config.y + config.height + config.width,
            tiledWidth: config.width,
            tiledHeight: config.height,
            x: config.x,
            y: config.y,
            typee: 'overlap-area',
            texture: 'default',
            frame: 0,
            flipX: false,
            flipY: false,
            depth: 0,
            rotation: 0,
            scrollFactor: 1,
            visible: true,
            originX: 0,
            originY: 0,
            width: config.width,
            height: config.height,
            body: true,
            moveable: false,
            tint: 0,
            level: config.level,
            popupText: ''
        })
        this.level = config.level
        this.overlapped = false
        this.overlaptmp = false
        this.deltabuff = 0
        this.enterCallback = () => {
            console.log("entering")
            this.emit(config.emit)
        }
        this.leaveCallback = () => {
            console.log("leaving")
            this.emit(config.emit)
        }
        this.scene.events.on('update', this.updateCallback.bind(this))
    }

    updateCallback(_sys: Phaser.Scenes.Systems, delta: number) {
        this.deltabuff += delta

        if (this.deltabuff>1000/10) {
            this.deltabuff = 0

            this.overlaptmp = this.level.scene.physics.world.overlap(this, this.level.player)
            if (
                this.overlaptmp && !this.overlapped
            ) {
                this.overlapped = true
                this.enterCallback()
            } else if (
                !this.overlaptmp && this.overlapped
            ) {
                this.overlapped = false
                this.leaveCallback()
            }

        }
    }
}
