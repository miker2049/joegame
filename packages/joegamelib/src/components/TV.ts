// import 'phaser'
// import MapObject from './MapObject'
// import BaseScene from './BaseScene';
// import { joegameData } from './joegameData';
// import Level from './Level';

// export default class TV extends Phaser.GameObjects.Sprite {
//     video: Phaser.GameObjects.Video;
//     scene: Level;

//     constructor(scene: BaseScene,x: number, y: number) {
//         super(scene, x, y, joegameData.textures.tv);
//         this.video = this.initVideo('home-movie')
//         this.video.setPipeline(joegameData.globalPipeline)
//         this.setPipeline(joegameData.globalPipeline)
//         this.scene.physics.world.enableBody(this, Phaser.Physics.Arcade.STATIC_BODY)
//         this.body.setSize(this.width-8,this.height/4).setOffset(3,16)
//         this.registerColliders();
//         this.setDepth(700)
//         this.video.setDepth(600)

//         this.scene.events.on('create',()=>{this.initColliders()})
//     }

//     initColliders(){
//         this.scene.physics.world.addCollider(this,this.scene.npcGroup)
//         this.scene.physics.world.addCollider(this,this.scene.player)
//     }

//     initVideo(vid: string): Phaser.GameObjects.Video {
//         if(this.video?.active){this.video.destroy()}
//         const video=this.scene.add.video(this.x,this.y-10,vid)
//         video.play(true,0,3000).setDisplaySize(this.width-10,this.height/2);
//         return video
//     }

//     registerColliders(){
//         const tiles =this.scene.map.tilemap.getTilesWithinWorldXY(this.body.x,this.body.y,this.body.width,this.body.height,{},this.scene.cameras.main,this.scene.map.mainLayer)
//         // this.scene.map.highlightTile(11,11)
//         tiles.forEach((tile)=>{this.scene.map.pathfinder.finder.avoidAdditionalPoint(tile.x,tile.y)})
//         console.log("TVS TILES",tiles);
//     }
// }
