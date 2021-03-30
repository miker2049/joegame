import 'phaser';
import './SceneMap';
import MapObject from './MapObject';
import SceneMap from './SceneMap';
export default class Door extends MapObject {
    constructor(scenemap: SceneMap, x: number, y: number, t_obj: Phaser.Types.Tilemaps.TiledObject);
    collideWith(obj: Phaser.GameObjects.GameObject): void;
    openDoor(): void;
}
//# sourceMappingURL=Door.d.ts.map