import 'phaser';
import {ILevelComponents, IMap} from '../ILevel'

export interface ITiledMapObject extends Phaser.Types.Tilemaps.TiledObject {
    depth: number
    texture?: string
}

export interface IMapObject extends Phaser.GameObjects.GameObject {
    name: string
    id: number
    tiledWidth: number
    tiledHeight: number
    props: object
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
export class MapObject extends Phaser.GameObjects.Sprite implements IMapObject {
    name: string
    id: number
    tiledWidth: number
    tiledHeight: number
    props: object

    /**
     * @param {ILevelComponents} level
     * @param {x} number
     * @param {y} number
     * @param {t_b} number
     */
    constructor(level: ILevelComponents, x: number, y: number, t_obj: ITiledMapObject  ) {
        super(level.scene, x, y, '');
        // this.setPipeline(joegameData.globalPipeline);
        this.props = {};
        this.name = t_obj.name || `${this.x.toString()}+${this.x.toString()}`;
        this.id = t_obj.id;
        this.tiledWidth = t_obj.width || 2
        this.tiledHeight = t_obj.height || 2
        if(!t_obj.properties){
            // console.log(`SCENEMAP/TILEDOBJECTS: ${this.name} does not have ANY defined properties, btw`)
        } else {
            for (let prop of t_obj.properties){
                this.setData(prop.name, prop.value)
            }
        }

        let wikiobj=this.scene.game.cache.json.get('gdata').mapobject.get(t_obj.type)

        if (t_obj.gid != undefined){
            if (this.scene.textures.exists(t_obj.gid.toString())){
                this.setTexture(t_obj.gid.toString());
            } else {
                //if there is a gid but not a texture itself, its in one of the tilesheets/spritemaps
                const found=level.map.tilesets.find((tset,ind,tsets)=>{
                    // is the gid in question equal to or over this sets first gid? Ok, is it beneath the next one, or already on the last one?
                    return t_obj.gid! >= tset.firstgid && tsets[ind+1] ? t_obj.gid!<tsets[ind+1]?.firstgid : true
                });
                if (found) {
                    this.setTexture(found.name, t_obj.gid - found.firstgid)
                }
            }
        } else if (t_obj.texture) {
            this.setTexture(t_obj.texture)
        } else if (wikiobj){
            this.setTexture(wikiobj.req_spritesheet[0])
        }



        this.setFlipX(t_obj.flippedHorizontal || false)
        this.setFlipY(t_obj.flippedVertical || false)
        this.setDepth(t_obj.depth)
        this.setRotation(Phaser.Math.DegToRad(t_obj.rotation || 0))
        this.setOrigin(0, 1);
        this.setDisplaySize(this.tiledWidth, this.tiledHeight);
        this.setSize(this.tiledWidth, this.tiledHeight);
        if (this.getData('body') || false) {
            const bodytype = this.getData('moveable') ? Phaser.Physics.Arcade.DYNAMIC_BODY : Phaser.Physics.Arcade.STATIC_BODY
            this.scene.physics.world.enableBody(this, bodytype)
        }
        if (this.getData('scrollFactor') || false) {
            const sf = this.getData('scrollFactor')
            this.setScrollFactor(sf)
        }
        if (this.getData('tint') || false) {
            const raw = this.getData('tint')
            const t = Phaser.Display.Color.HexStringToColor(raw.substring(3, 9))
            this.setTint(t.color)
        }
        // this.setSize(this.width,this.height);
        this.setVisible(t_obj.visible || true);
        // console.log(`${this.name} is being created!`);
        this.scene.events.addListener(`play_anim_${t_obj.name}`, () => { this.playAnim() });
        this.scene.events.addListener(`stop_anim_${t_obj.name}`, () => { this.stopAnim() });
        this.scene.events.addListener(this.getData('animHook') || '', () => { this.playAnim() });

        if (this.getData("playAnim")){
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
