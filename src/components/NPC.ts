import 'phaser';
import { Character, CharacterConfig } from './Character';
import { Machine, interpret, Interpreter} from 'xstate';
import NPCMachine from './NPCMachine';
import { machineOptions } from './NPCMachineOptions';
import { CharMoveAnims, CharStates, Dir } from './joegameData';
import Player from './Player';
import { nameList } from './random-name-list'
import SpeechBox from './SpeechBox';
type Vec2 = Phaser.Types.Math.Vector2Like;

export default class NPC extends Character {
    machine: Interpreter<any>;
    interests: Array<{x: number; y: number; finalFacing?: Dir}>
    currInterest: number;
    patience: number;
    name: string;
    speechbox: SpeechBox;
    auto: boolean;


    constructor(scene: Phaser.Scene, x = 0, y = 0, config: CharacterConfig, interests: Array<{x: number; y: number; finalFacing?: Dir} | Phaser.Tilemaps.Tile>) {
        super( scene, x, y, config);
        this.interests = interests;
        this.currInterest = 0;
        // const machOpts = machineOptions(this);
        this.patience = 1000;
        this.speed = 12;
        const mach = NPCMachine.withContext({
            char: this,
            interestCounter: 1,
            additionalAvoid: {x: 1, y: 1},
            auto: true,
            patience: 2000,
            interests: interests
        })
        this.machine = interpret(mach)
        this.auto = true;
        // this.setOrigin(0.5,0.85);
        // let thisName: string;
        // if (name){
        //     thisName = name;
        //     this.name = name;
        // } else {
        //     thisName = nameList[Math.floor(nameList.length * Math.random())]
        //     this.name = thisName;
        // }
        this.label.text = this.name;

        this.scene.npcGroup.add(this,true);
        this.scene.npcs.set(this.name,this);

        this.speechbox = new SpeechBox(this.scene, this);
        this.add(this.speechbox)

        //TODO this will be changed to pushable on change to stable 3.50
        this.body.setImmovable(true)
        // this.body.setPushable(false)

        this.scene.game.events.once('levelloaded',()=>{
            this.initColliders();
            this.machine.start();
        })
        this.scene.events.once('levelclosed',()=>{
            // this.initColliders();
            // this.machine.start();
            console.log("this should stop machine")
            this.machine.stop()
        })
        // this.scene.events.once(Phaser.Core.Events.DESTROY,()=>{
        //     this.machine.stop()
        // })
        // this.scene.events.once(Phaser.Scenes.Events.DESTROY,()=>{
        //     console.log("this should be stopping NPC machines!")
        //     this.machine.stop()
        // })
        // this.scene.events.once(Phaser.Scenes.Events.PAUSE,()=>{
        //     this.machine.stop()
        // })
    }

    idlingAction(){
        if (this.auto) {
            this.scene.time.addEvent({
                callback: ()=>{
                    this.currInterest = (this.currInterest + 1) % this.interests.length;
                    this.machine.send('MOVE_THOUGHT',{location:{
                        x: this.interests[this.currInterest].x,
                        y: this.interests[this.currInterest].y,
                        finalFacing: Dir.north}})
                },
                delay: this.patience
            });
        }
    }

    initColliders(){
        // TODO breaks switching level
        // this.scene.physics.add.collider(this, this.scene.map.mainLayer, (npc,wall)=>{
        //     this.machine.send('WALL_BUMP');
        //     // console.log('WALL_BUMP');
        // });
        // // with the player
        // this.scene.physics.add.collider(this,this.scene.player,(npc: NPC, player: Player)=>{
        //     // console.log('PLAYER_BUMP');
        //     // console.log(npc);
        //     // player.stopMove();
        //     npc.machine.send({type:'BUMP', sprite: player})
        // });
        // // with each other
        // this.scene.physics.add.collider(this,this.scene.npcGroup,(npc: NPC, npc_: NPC)=>{
        //     npc_.machine.send({type:'BUMP', sprite: npc})
        //     npc.machine.send({type:'BUMP', sprite: npc_})
        // });
    }
}
