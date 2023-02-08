import 'phaser'
// import { ILevelComponents } from '../ILevel';
// import { ITiledMapObject } from './MapObject';
// import { Overlapper, OverlapperPressEvent } from './Overlapper';

// /**
//  * MapItems are MapObjects the player picks up and is added to inventory
//  */
// export default class MapItem extends Overlapper {

//     floatTween: Phaser.Tweens.Tween
//     body: Phaser.Physics.Arcade.Body
//     soundCB: ()=>void

//     constructor(level: ILevelComponents, x: number, y: number, t_obj: ITiledMapObject  ) {
//         super(level, x, y, t_obj);

//         this.body =  new Phaser.Physics.Arcade.Body(this.scene.physics.world,this)
//         this.scene.physics.world.enableBody(this)
//         this.overlapMachine.start()
//         if (level.player) {
//             this.scene.physics.world.addOverlap(this, level.player, ()=>{
//                 this.overlapMachine.send('PRESS',{char: level.player})
//                 console.log('pressss')
//             })
//         }
//         this.soundCB =()=> level.toner.play({inst:"itemPickup"})

//         this.floatTween=this.scene.tweens.add({
//             targets: [this],
//             y: this.y-3,
//             ease: 'Sine',
//             loop: -1,
//             duration: 2000,
//             yoyo: true
//         })
//     }

//     pressedCallback(context, event: OverlapperPressEvent){
//         this.floatTween.remove()

//         this.body.setVelocity(0)
//         this.soundCB()

//         floatTo(this,event.char,1).then(()=>{
//             this.destroy()
//         })
//     }
// }
