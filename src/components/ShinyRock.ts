import 'phaser'
import MapObject from './MapObject';
import Tooltip from './Tooltip';
import SceneMap from './SceneMap';
import ProgressBar from './ProgressBar';
import approachArea from './approachArea';

import { interpret, Interpreter, Machine, MachineConfig } from 'xstate';

export default class ShinyRock extends MapObject {
    engraved: boolean;
    ichingframe: number;
    progress: ProgressBar;
    machine: Interpreter<any>;
    constructor(scenemap: SceneMap,x: number, y: number, t_obj: Phaser.Types.Tilemaps.TiledObject  ){
        super(scenemap,x,y,t_obj)
        this.engraved = false;
        this.ichingframe = Math.floor(Math.random()*this.scene.textures.get('iching').frameTotal)
        this.progress=new ProgressBar(this.scene, this.x+this.width/2, this.y-this.height,32,0.01)
        this.machine = interpret(Machine(ShinyRockMachineConfig,{
            actions:{
                addEngraving:()=>{
                    this.scene.add.image(this.x+this.width/2,this.y-(this.height/2*0.9),'iching',this.ichingframe).setScale(0.155).setDepth(this.depth+1)
                    this.progress.destroy()
                    this.scene.tweens.add({
                        targets:[this.scene.cameras.main],
                        zoom: "+=2",
                        duration: 4000,
                        yoyo:true,
                        delay: 300
                    });
                }
            },
            guards:{
                done:()=>this.progress.done
            }
        }))
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

const ShinyRockMachineConfig: MachineConfig<any,any,any>={
    initial: 'alone',
    states:{
        alone:{
            on:{
                APPROACHED: 'selected'
            }
        },
        selected:{
            // entry: [''],
            on:{
                HIT:[
                    {target:'engraved', cond: 'done'},
                    'selected'
                ]
            }
        },
        engraved:{
            type: 'final',
            entry: ['addEngraving']
        }
    }
}
