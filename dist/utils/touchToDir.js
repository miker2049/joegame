import 'phaser';
import { Dir } from '../joegameTypes';
export default function (coll) {
    if (coll.up) {
        return Dir.south;
    }
    else if (coll.down) {
        return Dir.north;
    }
    else if (coll.right) {
        return Dir.west;
    }
    else if (coll.left) {
        return Dir.east;
    }
    return Dir.south;
}
//# sourceMappingURL=touchToDir.js.map