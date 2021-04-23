import 'phaser';
import { joegameData } from './joegameData';
export default class TV extends Phaser.GameObjects.Sprite {
    constructor(scene, x, y) {
        super(scene, x, y, joegameData.textures.tv);
        this.video = this.initVideo('home-movie');
        this.video.setPipeline(joegameData.globalPipeline);
        this.setPipeline(joegameData.globalPipeline);
        this.scene.physics.world.enableBody(this, Phaser.Physics.Arcade.STATIC_BODY);
        this.body.setSize(this.width - 8, this.height / 4).setOffset(3, 16);
        this.registerColliders();
        this.setDepth(700);
        this.video.setDepth(600);
        this.scene.events.on('create', () => { this.initColliders(); });
    }
    initColliders() {
        this.scene.physics.world.addCollider(this, this.scene.npcGroup);
        this.scene.physics.world.addCollider(this, this.scene.player);
    }
    initVideo(vid) {
        var _a;
        if ((_a = this.video) === null || _a === void 0 ? void 0 : _a.active) {
            this.video.destroy();
        }
        const video = this.scene.add.video(this.x, this.y - 10, vid);
        video.play(true, 0, 3000).setDisplaySize(this.width - 10, this.height / 2);
        return video;
    }
    registerColliders() {
        const tiles = this.scene.map.tilemap.getTilesWithinWorldXY(this.body.x, this.body.y, this.body.width, this.body.height, {}, this.scene.cameras.main, this.scene.map.mainLayer);
        // this.scene.map.highlightTile(11,11)
        tiles.forEach((tile) => { this.scene.map.pathfinder.finder.avoidAdditionalPoint(tile.x, tile.y); });
        console.log("TVS TILES", tiles);
    }
}
//# sourceMappingURL=TV.js.map