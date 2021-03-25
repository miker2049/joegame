import { getSceneKeyName } from './utils/getKeyNames';
import createTilemap from './factories/createTilemap';
import createPathfinder from './factories/createPathfinder';
import { MachineRegistry } from './components/MachineRegistry';
export class Level {
    constructor(game, mapjsonpath) {
        this.key = mapjsonpath ? mapjsonpath : 'empty';
        this.scene = game.scene.add(getSceneKeyName(this.key), new Phaser.Scene(getSceneKeyName(this.key)), true);
        this.npcs = this.scene.physics.add.group();
        this.platforms = this.scene.physics.add.group();
        this.map = createTilemap(this.scene, this.key);
        this.pathfinder = createPathfinder(this.map);
        this.machineRegisty = new MachineRegistry();
    }
}
//# sourceMappingURL=Level.js.map