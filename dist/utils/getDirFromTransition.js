import { Dir } from '../joegameTypes';
export default function (pathTransition) {
    if (pathTransition.x === 1 && pathTransition.y === 0) {
        return Dir.east;
    }
    else if (pathTransition.x === -1 && pathTransition.y === 0) {
        return Dir.west;
    }
    else if (pathTransition.x === 0 && pathTransition.y === 1) {
        return Dir.south;
    }
    else if (pathTransition.x === 0 && pathTransition.y === -1) {
        return Dir.north;
    }
    else {
        return Dir.north;
    }
}
//# sourceMappingURL=getDirFromTransition.js.map