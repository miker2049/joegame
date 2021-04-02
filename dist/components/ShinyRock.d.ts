import 'phaser';
import MapObject from './MapObject';
import SceneMap from './SceneMap';
import ProgressBar from './ProgressBar';
import { Interpreter } from 'xstate';
export default class ShinyRock extends MapObject {
    engraved: boolean;
    ichingframe: number;
    progress: ProgressBar;
    machine: Interpreter<any>;
    constructor(scenemap: SceneMap, x: number, y: number, t_obj: Phaser.Types.Tilemaps.TiledObject);
}
