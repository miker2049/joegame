import 'phaser';
import ensure from './ensure';
export default function (path) {
    let transitions = [];
    for (let i = 1; i < path.length; i++) {
        const xDiff = ensure(path[i].x) - ensure(path[i - 1].x);
        const yDiff = ensure(path[i].y) - ensure(path[i - 1].y);
        transitions.push({ x: xDiff, y: yDiff });
    }
    return transitions;
}
//# sourceMappingURL=createPathTransitions.js.map