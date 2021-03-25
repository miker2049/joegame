import 'phaser';
import MapObject from './MapObject';
export default class OverlapArea extends MapObject {
    constructor(scenemap, x, y, t_obj) {
        super(scenemap, x, y, t_obj);
        this.callback = () => { };
        const cb_ = this.getData('cb');
        if (cb_) {
            this.callback = () => { this.scene.events.emit(cb_); };
        }
        else {
            // console.log("No callback defined for overlap area ${this.name}");
        }
        this.scene.events.addListener(`activate_${t_obj.name}`, () => { this.activateOverlap(this.scene.player); });
        if (this.getData("active")) {
            this.scene.events.on('create', () => {
                this.activateOverlap(this.scene.player);
            });
        }
    }
    activateOverlap(player) {
        const overlap_id = this.name;
        this.playAnim();
        this.scene.physics.world.enableBody(this);
        this.setDisplaySize(this.tiledWidth, this.tiledHeight);
        this.setSize(this.tiledWidth, this.tiledHeight);
        this.scene.physics.add.overlap(this, player, () => {
            this.scene.physics.world.colliders.getActive().find(function (i) {
                return i.name == overlap_id;
            }).destroy();
            this.setVisible(false);
            this.callback();
            this.destroy();
        }, () => { }, this.scene).name = overlap_id;
    }
}
//# sourceMappingURL=OverlapArea.js.map