import 'phaser';
import {IMap} from '../ILevel'

export interface ITiledMapObject extends Phaser.Types.Tilemaps.TiledObject {
    depth: number
}

export interface IMapObject {
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

export class MapObject extends Phaser.GameObjects.Sprite implements IMapObject {
    name: string
    id: number
    tiledWidth: number
    tiledHeight: number
    props: object

    constructor(scene: Phaser.Scene, tilemap: IMap,x: number, y: number, t_obj: ITiledMapObject  ) {
        super(scene, x, y, '');
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
        if (t_obj.gid != undefined){
            if (this.scene.textures.exists(t_obj.gid.toString())){
                this.setTexture(t_obj.gid.toString());
            } else {
                //if there is a gid but not a texture itself, its in one of the tilesheets/spritemaps
                const found=tilemap.tilesets.find((tset,ind,tsets)=>{
                    // is the gid in question equal to or over this sets first gid? Ok, is it beneath the next one, or already on the last one?
                    return t_obj.gid! >= tset.firstgid && tsets[ind+1] ? t_obj.gid!<tsets[ind+1]?.firstgid : true
                });
                if (found) {
                    this.setTexture(found.name, t_obj.gid - found.firstgid)
                }
            }
        }

        this.setFlipX(t_obj.flippedHorizontal || false)
        this.setFlipY(t_obj.flippedVertical || false)
        this.setDepth(t_obj.depth)
        this.setRotation(Phaser.Math.DegToRad(t_obj.rotation || 0))
        this.setOrigin(0,1);
        this.setDisplaySize(this.tiledWidth,this.tiledHeight);
        this.setSize(this.tiledWidth,this.tiledHeight);
        if(this.getData('body') || false) {
            const bodytype = this.getData('moveable') ? Phaser.Physics.Arcade.DYNAMIC_BODY : Phaser.Physics.Arcade.STATIC_BODY
            this.scene.physics.world.enableBody(this,bodytype)
            // scenemap.objbody.add(this)
        }
        if(this.getData('scrollFactor') || false) {
            const sf = this.getData('scrollFactor')
            this.setScrollFactor(sf)
            // scenemap.objbody.add(this)
        }
        // this.setSize(this.width,this.height);
        this.setVisible(t_obj.visible || true);
        // console.log(`${this.name} is being created!`);
        this.scene.events.addListener(`play_anim_${t_obj.name}`,()=>{this.playAnim()});
        this.scene.events.addListener(`stop_anim_${t_obj.name}`,()=>{this.stopAnim()});
        this.scene.events.addListener(this.getData('animHook') || '',()=>{this.playAnim()});

    }

    playAnim(){
        const anim_ =this.getData('anim');
        if(anim_){
            this.anims.play(anim_);
            this.setDisplaySize(this.width, this.height)
        } else {
            "No anim set on the ${this.name} tiled object (or elsewhere!)"
        }
    }
    stopAnim(){
        this.anims.stop();
    }
}
