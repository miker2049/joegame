import 'phaser'
import BaseScene from './BaseScene';
import OverlapArea from './OverlapArea';
import Level from './Level';
import { joegameData } from './joegameData';
import SceneMap from './SceneMap';

/**
 * MapItems are MapObjects the player picks up and is added to inventory
 */
export default class MapItem extends OverlapArea{
    scene: Level;
    sparkles: Phaser.GameObjects.Particles.ParticleEmitterManager;
    constructor(scenemap: SceneMap,x: number, y: number, t_obj: Phaser.Types.Tilemaps.TiledObject  ) {
        super(scenemap, x, y, t_obj);
        this.setOrigin(0,1)
        const itemData = joegameData.items[this.name];
        this.setTexture(itemData.texture,itemData.frame || 0).setScale(itemData.scale || 1)
        this.callback=()=>{
            this.scene.registry.get('Inventory').push(this.name);
            this.scene.notify(`you picked up ${this.name}`)
            this.sparkles.destroy()
            this.destroy()
        }
        this.activateOverlap(this.scene.player);
        this.sparkles = this.scene.add.particles(itemData.particleTexture||'yellow-particle');
        if (itemData.sparkly){
            this.activateSparkles()
        }
        this.scene.tweens.add({
            targets: [this],
            y: this.y-1,
            ease: 'Sine',
            loop: -1,
            duration: 2000,
            yoyo: true
        })
    }

    activateSparkles(){
        this.sparkles.createEmitter({})
            .setPosition(this.x+this.width/2, this.y-this.height/2)
            .setBlendMode(Phaser.BlendModes.ADD)
            .setSpeed(10)
        .setScale(0.5)
        .setLifespan(500)
    }
}
