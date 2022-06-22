import 'phaser';
import './SceneMap';
import {MapObject} from './MapObject';

export default class Door extends MapObject {
    // constructor(scenemap: SceneMap,x: number, y: number, t_obj: Phaser.Types.Tilemaps.TiledObject  ) {
    //     super(scenemap, x, y, t_obj);
    //     this.setTexture(joegameData.DoorTexture);
    //     this.setDisplaySize(this.tiledWidth, this.tiledHeight);
    //     this.scene.events.addListener(`open_${t_obj.name}`,()=>{this.openDoor()});
    //     this.scene.physics.add.existing(this, true);
    // }

    // collideWith(obj:Phaser.GameObjects.GameObject){
    //     this.scene.physics.add.collider(this,obj,()=>{console.log("hieeeee")},undefined,this.scene);
    // }

    // openDoor(){
    //     if (this.tiledWidth < this.tiledHeight) { //then its a door opening vertically
    //         this.scene.tweens.add({
    //             targets: this,
    //             scaleY: { from: this.scaleY, to: 0 },
    //             duration: 2010,
    //             repeat:0,
    //             onComplete: () => {this.destroy(true)}
    //         });
    //         this.scene.tweens.add({
    //             targets: this.body,
    //             height: { from: this.body.height, to: 1 },
    //             duration: 2000,
    //             repeat:0,
    //         });
    //     } else {
    //         this.scene.tweens.add({
    //             targets: this,
    //             scaleX: { from: this.scaleX, to: 0 },
    //             duration: 2010,
    //             repeat:0,
    //             onComplete: () => {this.destroy(true)}
    //         });
    //         this.scene.tweens.add({
    //             targets: this.body,
    //             width: { from: this.body.width, to: 1 },
    //             duration: 2000,
    //             repeat:0,
    //         });
    //     }
    // }
}
