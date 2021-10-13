import { ILevelComponents, IMap } from 'ILevel';
import { ITiledMapObject, MapObject } from './MapObject';
import shaders from '../shaders/index'
import {ISwirlPipeline, SwirlPipeline} from '../shaders/SwirlPipeline'

export class CanyonSwirl extends MapObject {

    constructor(level: ILevelComponents, x: number, y: number, t_obj: ITiledMapObject) {
        super({level, x, y, t_obj})
        const pipelines = (this.scene.renderer as Phaser.Renderer.WebGL.WebGLRenderer).pipelines
        const swirl=pipelines.add(
            'swirl',
            new shaders.SwirlPipeline(level.scene.game)
        ) as SwirlPipeline
        this.setPipeline(
            'swirl',
            shaders.SwirlPipeline
        )
        const center = this.getCenter()
        swirl.setCenter(center.x, center.y)
        // console.log(x,y, this.getCenter())
        swirl.radius = this.width*2
        this.rotation = Phaser.Math.DegToRad(360*Math.random())
        this.setOrigin(0.5)
        this.setPosition(x+this.width/2)
        level.scene.tweens.add({
            targets: [swirl],
            rotation:{
                from: Phaser.Math.DegToRad(-180),
                to: Phaser.Math.DegToRad(180)
            },
            ease: 'Linear',
            duration: 60000 -( 20000*Math.random()),
            repeat: -1,
            yoyo: true
        })
        level.scene.tweens.add({
            targets: [this],
            rotation:{
                from: this.rotation,
                to: Phaser.Math.DegToRad(360) + this.rotation
            },
            ease: 'Linear',
            duration: 40000,
            repeat: -1
        })
    }
}
