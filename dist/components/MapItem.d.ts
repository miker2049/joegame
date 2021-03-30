import 'phaser';
import OverlapArea from './OverlapArea';
import Level from './Level';
import SceneMap from './SceneMap';
/**
 * MapItems are MapObjects the player picks up and is added to inventory
 */
export default class MapItem extends OverlapArea {
    scene: Level;
    sparkles: Phaser.GameObjects.Particles.ParticleEmitterManager;
    constructor(scenemap: SceneMap, x: number, y: number, t_obj: Phaser.Types.Tilemaps.TiledObject);
    activateSparkles(): void;
}
//# sourceMappingURL=MapItem.d.ts.map