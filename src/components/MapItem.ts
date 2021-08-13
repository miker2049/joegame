import 'phaser'
import {ILevelComponents} from '../ILevel'
import { ITiledMapObject, MapObject } from './MapObject';
import { IOverlapper, OverlapMachine, OverlapMachineEvents, Overlapper, OverlapperPressEvent } from './Overlapper';
import {interpret, Interpreter, Machine} from 'xstate'
import floatTo from 'actions/floatTo';

/**
 * MapItems are MapObjects the player picks up and is added to inventory
 */
export default class MapItem extends Overlapper {

    floatTween: Phaser.Tweens.Tween
    body: Phaser.Physics.Arcade.Body
    soundCB: ()=>void

    constructor(level: ILevelComponents, x: number, y: number, t_obj: ITiledMapObject  ) {
        super(level, x, y, t_obj);
        // this.overlapCallback=()=>{
        //     // this.scene.registry.get('Inventory').push(this.name);
        //     // this.scene.notify(`you picked up ${this.name}`)
        //     // this.sparkles.destroy()
        //     this.destroy()
        // }
        this.body =  new Phaser.Physics.Arcade.Body(this.scene.physics.world,this)
        this.scene.physics.world.enableBody(this)
        this.overlapMachine.start()
        if (level.player) {
            this.scene.physics.world.addOverlap(this, level.player, ()=>{
                this.overlapMachine.send('PRESS',{char: level.player})
                console.log('pressss')
            })
        }
        this.soundCB =()=> level.toner.play({inst:"itemPickup"})
        // this.pressedCallback = ()=>{

        // this.destroy()
        // }
        // this.activateOverlap(this.scene.player);
        // this.sparkles = this.scene.add.particles(itemData.particleTexture||'yellow-particle');
        // if (itemData.sparkly){
        //     this.activateSparkles()
        // }
        this.floatTween=this.scene.tweens.add({
            targets: [this],
            y: this.y-3,
            ease: 'Sine',
            loop: -1,
            duration: 2000,
            yoyo: true
        })
    }

    pressedCallback(context, event: OverlapperPressEvent){
        this.floatTween.remove()

        this.body.setVelocity(0)
        this.soundCB()

        floatTo(this,event.char,1).then(()=>{
            this.destroy()
        })
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
