import 'phaser'
import {ILevelComponents} from '../ILevel'
import { ITiledMapObject, MapObject } from './MapObject';
import { IOverlapper, OverlapMachine, OverlapMachineEvents, Overlapper } from './Overlapper';
import {interpret, Interpreter, Machine} from 'xstate'

/**
 * MapItems are MapObjects the player picks up and is added to inventory
 */
export default class MapItem extends Overlapper {

    constructor(level: ILevelComponents, x: number, y: number, t_obj: ITiledMapObject  ) {
        super(level, x, y, t_obj);
        // this.overlapCallback=()=>{
        //     // this.scene.registry.get('Inventory').push(this.name);
        //     // this.scene.notify(`you picked up ${this.name}`)
        //     // this.sparkles.destroy()
        //     this.destroy()
        // }

        if (level.player) {
            this.scene.physics.world.addOverlap(this, level.player, ()=>{
                this.overlapMachine.send('PRESS')
            })
        }
        // this.activateOverlap(this.scene.player);
        // this.sparkles = this.scene.add.particles(itemData.particleTexture||'yellow-particle');
        // if (itemData.sparkly){
        //     this.activateSparkles()
        // }
        this.scene.tweens.add({
            targets: [this],
            y: this.y-1,
            ease: 'Sine',
            loop: -1,
            duration: 2000,
            yoyo: true
        })
    }

    pressedCallback(){
        this.destroy()
    }

    // activateSparkles(){
    //     this.sparkles.createEmitter({})
    //         .setPosition(this.x+this.width/2, this.y-this.height/2)
    //         .setBlendMode(Phaser.BlendModes.ADD)
    //         .setSpeed(10)
    //     .setScale(0.5)
    //     .setLifespan(500)
    // }
}
