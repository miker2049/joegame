import 'phaser';
import MapObject from './MapObject';
import ProgressBar from './ProgressBar';
import { interpret, Machine } from 'xstate';
export default class ShinyRock extends MapObject {
    constructor(scenemap, x, y, t_obj) {
        super(scenemap, x, y, t_obj);
        this.engraved = false;
        this.ichingframe = Math.floor(Math.random() * this.scene.textures.get('iching').frameTotal);
        this.progress = new ProgressBar(this.scene, this.x + this.width / 2, this.y - this.height, 32, 0.01);
        this.machine = interpret(Machine(ShinyRockMachineConfig, {
            actions: {
                addEngraving: () => {
                    this.scene.add.image(this.x + this.width / 2, this.y - (this.height / 2 * 0.9), 'iching', this.ichingframe).setScale(0.155).setDepth(this.depth + 1);
                    this.progress.destroy();
                    this.scene.tweens.add({
                        targets: [this.scene.cameras.main],
                        zoom: "+=2",
                        duration: 4000,
                        yoyo: true,
                        delay: 300
                    });
                }
            },
            guards: {
                done: () => this.progress.done
            }
        }));
        this.machine.start();
        // new Tooltip(this.scene,this.x+this.width/2,this.y-this.height/2,this.width*1.5,"hold <Shift> to engrave")
        // this.scene.physics.world.addOverlap()
        //TODO breaks switching level
        // this.scene.events.on('create',()=>{
        //     this.scene.physics.world.addCollider(this,this.scene.player,()=>{
        //         this.progress.incrementProgress(0.2)
        //         console.log('hitittt')
        //         this.machine.send('HIT')
        //     })
        //     let cll = approachArea(this,this.scene.player,()=>{
        //         console.log("Approach!!")
        //         this.machine.send('APPROACHED')
        //         cll.destroy();
        //     })
        // });
    }
}
const ShinyRockMachineConfig = {
    initial: 'alone',
    states: {
        alone: {
            on: {
                APPROACHED: 'selected'
            }
        },
        selected: {
            // entry: [''],
            on: {
                HIT: [
                    { target: 'engraved', cond: 'done' },
                    'selected'
                ]
            }
        },
        engraved: {
            type: 'final',
            entry: ['addEngraving']
        }
    }
};
//# sourceMappingURL=ShinyRock.js.map