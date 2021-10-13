import 'phaser';
import { InputType } from 'zlib';
import { ILevelComponents, IMap } from '../ILevel'

export interface ITiledMapObject extends Phaser.Types.Tilemaps.TiledObject {
    depth: number
    texture?: string
}

export interface IMapObject extends Phaser.GameObjects.GameObject {
    name: string
    id: number
    tiledWidth: number
    tiledHeight: number
    playAnim(): void
    stopAnim(): void
    x: number
    y: number
}

/**
 * this in itself is a sprite component that gets all of its initialization from
 * the parsed Tiled map object, represented by `IMapObject`
 *
 * @extends Phaser.GameObjects.Sprite
 * @implements IMapObject
 * @property {string} name - the name of the object, which if not defined is "hashed" from start coordinates
 * @property {number} id - id
 * @property {number} tiledWidth
 * @property {number} tiledHeight
 * @property {object} props
 */

export interface IMapObjectConfig {
    name: string
    id: number
    x: number
    y: number
    tiledWidth: number
    tiledHeight: number
    typee: string
    texture: string
    frame: number
    flipX: boolean
    flipY: boolean
    depth: number
    rotation: number
    originX: number
    originY: number
    scrollFactor: number
    visible: boolean
    width: number
    height: number
    body: boolean
    moveable: boolean
    tint: number,
    level: ILevelComponents
    popupText: string
}

export class MapObject extends Phaser.GameObjects.Sprite implements IMapObject {
    name: string
    id: number
    tiledWidth: number
    tiledHeight: number

    constructor({ name, id, x, y, tiledWidth,
        tiledHeight, typee, texture, frame,
        flipX, flipY, depth, rotation, scrollFactor,
        visible, originX, originY, width, height,
        body, moveable, tint, level, popupText}: IMapObjectConfig) {
    super(level.scene, x, y, texture, frame);

    this.name = name
    this.id = id
    this.tiledWidth = tiledWidth
    this.tiledHeight = tiledHeight


    this.setFlipX(flipX)
    this.setFlipY(flipY)
    this.setDepth(depth)
    this.setRotation(Phaser.Math.DegToRad(rotation))
    this.setOrigin(originX, originY);
    this.setDisplaySize(this.tiledWidth, this.tiledHeight);
    this.setSize(this.tiledWidth, this.tiledHeight);
    if (body) {
        const bodytype = this.getData('moveable') ? Phaser.Physics.Arcade.DYNAMIC_BODY : Phaser.Physics.Arcade.STATIC_BODY
        this.scene.physics.world.enableBody(this, bodytype)
    }
        this.setScrollFactor(scrollFactor)
        this.setTint(tint)
    if (popupText.length > 0) {
        // const raw = this.getData('popupText')
        // const t = Phaser.Display.Color.HexStringToColor(raw.substring(3, 9))

        // this.setTint(t.color)
    }

    // this.setSize(this.width,this.height);
    this.setVisible(visible);
    // console.log(`${this.name} is being created!`);
    this.scene.events.addListener(`play_anim_${name}`, () => { this.playAnim() });
    this.scene.events.addListener(`stop_anim_${name}`, () => { this.stopAnim() });
    this.scene.events.addListener(this.getData('animHook') || '', () => { this.playAnim() });

    if (this.getData("playAnim")) {
        this.playAnim()
    }

}

playAnim() {
    const anim_ = this.getData('anim');
    if (anim_) {
        this.anims.play(anim_);
        this.setDisplaySize(this.width, this.height)
    } else {
        "No anim set on the ${this.name} tiled object (or elsewhere!)"
    }
}
stopAnim() {
    this.anims.stop();
}
}
