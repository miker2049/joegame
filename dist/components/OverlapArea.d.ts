import 'phaser';
import MapObject from './MapObject';
import Character from './Character';
import SceneMap from './SceneMap';
export default class OverlapArea extends MapObject {
    constructor(scenemap: SceneMap, x: number, y: number, t_obj: Phaser.Types.Tilemaps.TiledObject);
    activateOverlap(player: Character): void;
    callback: Function;
}
//# sourceMappingURL=OverlapArea.d.ts.map