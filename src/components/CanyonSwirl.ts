import { IMap } from 'ILevel';
import { ITiledMapObject, MapObject } from './MapObject';
import shaders from '../shaders/index'

export class CanyonSwirl extends MapObject {
    constructor(scene: Phaser.Scene, tilemap: IMap, x: number, y: number, t_obj: ITiledMapObject) {
        super(scene, tilemap, x, y, t_obj)
        const pipelines = (this.scene.renderer as Phaser.Renderer.WebGL.WebGLRenderer).pipelines
        const swirl=pipelines.add(
            'swirl',
            new shaders.SwirlPipeline(scene.game)
        )
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
        scene.tweens.add({
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
        scene.tweens.add({
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
