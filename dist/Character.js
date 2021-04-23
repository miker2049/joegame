import { __awaiter } from "tslib";
import 'phaser';
import jumpUp from './actions/charJumpUp';
import { Dir, VelocityMap } from './joegameTypes';
import getDashVelForDistance from './utils/getDashVelForDistance';
import defaults from './defaults';
import createResidualGraphic from './actions/createResidualGraphic';
import VoxBox from './components/VoxBox';
import speakString from './actions/speakString';
export default class Character extends Phaser.GameObjects.Container {
    constructor(config) {
        var _a, _b, _c, _d;
        super(config.level.scene, config.x, config.y);
        this.level = config.level;
        //assign settings from config
        this.speed = config.speed;
        this.name = config.name;
        this.dashDistance = config.dashDistance;
        this.charge = 2;
        this.animKeys = config.anims;
        this.dashDrag = 300;
        this.dashVel = getDashVelForDistance(this.dashDistance, this.dashDrag);
        this.sprite = config.level.scene.make.sprite({ key: config.texture }, false);
        this.sprite.removeFromDisplayList();
        this.voxbox = new VoxBox(this.level);
        this.voxbox.close();
        this.setDepth(this.depth = config.depth || defaults.charDepth);
        this.face(Dir.south);
        this.facing = Dir.south;
        this.onPlatform = false;
        this.auto = false;
        this.player = false;
        this.groundVel = { x: 0, y: 0 };
        // this.sprite.setTintFill(Phaser.Display.Color.RandomRGB().color)
        // this.setSize(this.scene.tileWidth/2,this.scene.tileHeight/2)
        this.sprite.setScale(config.scale);
        this.setInteractive(new Phaser.Geom.Circle(0, 0, this.level.map.tileWidth * 2), Phaser.Geom.Circle.Contains);
        this.level.scene.physics.world.enable(this, Phaser.Physics.Arcade.DYNAMIC_BODY);
        this.charBody = this.body;
        this.add(this.sprite);
        this.charBody.setSize(((_a = config.body) === null || _a === void 0 ? void 0 : _a.width) || this.level.map.tileWidth * 0.5, ((_b = config.body) === null || _b === void 0 ? void 0 : _b.height) || this.level.map.tileHeight * 0.5);
        this.sprite.setPosition(this.charBody.width / 2, 0);
        this.charBody.setOffset(((_c = config.body) === null || _c === void 0 ? void 0 : _c.offsetX) || 0, ((_d = config.body) === null || _d === void 0 ? void 0 : _d.offsetY) || 0);
        this.add(this.voxbox);
        // this.sprite.on('animationstart', (anim, frame) => { console.log(`start of ${anim}`) })
        // this.sprite.on('animationrepeat', (anim, frame) => { console.log(`update of ${anim}`) })
        // this.sprite.on('animationupdate', (anim, frame) => {
        //     console.log(`is ${anim}, frame is ${frame}`)
        // })
        // let pnt=this.scene.make.graphics({x:0,y:0}).fillStyle(Phaser.Display.Color.GetColor(255,0,0)).fillCircle(this.charBody.width/2,this.charBody.height/2,1).setDepth(10)
        // let pnt2=this.scene.make.graphics({x:0,y:0}).fillStyle(Phaser.Display.Color.GetColor(0,255,0)).fillCircle(this.charBody.top,0,1).setDepth(10)
        // let pnt=this.scene.make.graphics({x:0,y:0}).fillStyle(Phaser.Display.Color.GetColor(255,0,0)).fillCircle(0,0,1).setDepth(10)
        // this.add([pnt2])
    }
    //control
    changeGroundVel() {
        let platform;
        this.scene.physics.world.overlap(this, this.level.platforms, (player, plat) => {
            platform = plat;
            if (platform != undefined) {
                this.groundVel = { x: platform.body.velocity.x, y: platform.body.velocity.y };
            }
            else {
                this.groundVel = { x: 0, y: 0 };
            }
        });
    }
    move(dir) {
        this.charBody.allowDrag = false;
        this.facing = dir;
        this.playAnim(this.animKeys[Dir[dir]]);
        this.charBody.setVelocity(this.groundVel.x || 0, this.groundVel.y || 0); // this is needed again to cancel diagonole shit
        this.charBody.velocity.add(new Phaser.Math.Vector2(this.speed * VelocityMap[dir][0], this.speed * VelocityMap[dir][1]));
    }
    dash(dir) {
        this.showDashboxes();
        this.charBody.allowDrag = true;
        this.charBody.setDrag(this.dashDrag, this.dashDrag);
        this.charBody.setVelocity(0);
        this.charBody.setAcceleration(0, 0);
        this.face(dir);
        this.stopAnim();
        this.charBody.setVelocity(this.dashVel * VelocityMap[dir][0] + (this.groundVel.x || 0), this.dashVel * VelocityMap[dir][1] + (this.groundVel.y || 0));
        createResidualGraphic(this.sprite, this.x, this.y);
    }
    face(dir) {
        const dirAnim = this.scene.anims.get(this.animKeys[dir]);
        //NOTE this if should be unecessary
        if (dirAnim) {
            this.sprite.anims.setCurrentFrame(dirAnim.frames[1]);
            this.facing = dir;
        }
    }
    stop(face) {
        this.charBody.allowDrag = false;
        this.charBody.setVelocity(this.groundVel.x || 0, this.groundVel.y || 0);
        this.stopAnim();
        if (face) {
            this.face(face);
            this.facing = face;
        }
    }
    align() {
        const x_ = (Math.floor((this.charBody.center.x) / this.level.map.tileWidth) * this.level.map.tileWidth);
        const y_ = (Math.floor((this.charBody.center.y) / this.level.map.tileHeight) * this.level.map.tileHeight);
        this.transport(x_ + this.level.map.tileWidth / 2, y_ + this.level.map.tileHeight / 2);
        // this.transport(x_, y_)
        return { x: x_ / this.level.map.tileWidth, y: (y_ / this.level.map.tileHeight) };
    }
    jumpUp() {
        jumpUp(this);
    }
    /*
     * The dir here marks the direction where you are jumping back from
     */
    jumpBack(dir) {
        console.log(this.name + " is jumping!");
        this.jumpUp();
        switch (dir) {
            //if collider.body.touching.up
            case Dir.north: {
                this.face(Dir.north);
                this.transportNudge(0, 8);
                this.align();
                break;
            }
            case Dir.south: {
                this.face(Dir.south);
                this.transportNudge(0, -8);
                this.align();
                break;
            }
            case Dir.east: {
                this.face(Dir.east);
                this.transportNudge(-8, 0);
                this.align();
                break;
            }
            case Dir.west: {
                this.face(Dir.west);
                this.transportNudge(8, 0);
                this.align();
                break;
            }
        }
    }
    transport(x, y) { this.setPosition(x + (this.x - this.charBody.center.x), y + (this.y - this.charBody.center.y)); }
    transportNudge(x, y) { this.setPosition(this.x + x, this.y + y); }
    minusCharge() { }
    playAnim(anim) {
        this.sprite.anims.play({ key: anim, delay: 0, repeat: -1, frameRate: 11, startFrame: 1 }, false);
    }
    stopAnim() {
        this.sprite.anims.stop();
        const dirAnim = this.scene.anims.get(this.animKeys[this.facing]);
        this.sprite.anims.setCurrentFrame(dirAnim.frames[1]);
    }
    // UI control
    showDashboxes() { }
    minusDashbox() { }
    showLabel() { }
    hideLabel() { }
    speak(msg, speed) {
        return __awaiter(this, void 0, void 0, function* () {
            yield Promise.all([this.voxbox.speak(msg), speakString(msg, this, (config) => this.level.toner.play(config))]);
            return;
            // console.log(msg)
        });
    }
}
//# sourceMappingURL=Character.js.map