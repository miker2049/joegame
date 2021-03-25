import 'phaser';
import { Dir } from '../joegameTypes';
import ensure from '../utils/ensure';
function moveOnPath(params) {
    // this.char.body.allowDrag = false;
    let tweens = [];
    for (let transition of params.pathTransitions) {
        tweens.push(getTileMoveTween(transition, params.charControl, params.tileSize, params.speed));
    }
    tweens.push({
        targets: [params.charControl.charBody],
        duration: 100,
        onComplete: () => {
            if (params.finalFacing != undefined) {
                params.charControl.face(params.finalFacing);
            }
            params.charControl.stop();
            params.cb;
        },
    });
    return params.tweener.timeline({ tweens: tweens });
}
export function moveToTile(params) {
    let timeline = 'NO_PATH';
    // destroy the current tween timeline that is running
    // this.char.destTimeline.destroy();
    //This janky movement doesnt seem to matter so much when you start moving and animating, makes sure the distance calculations are correct too
    params.charController.align();
    // get the players current tile
    // here the callback first takes the path and makes it in to a bunch of (-1/0/1,-1/0/1) type transitions, then puts that into the path function
    if (params.tempObsX != undefined && params.tempObsY != undefined) {
        params.finder.avoidAdditionalPoint(params.tempObsX, params.tempObsY);
    }
    params.finder.findPath(params.x, params.y, params.dx, params.dy, (path) => {
        console.log(path);
        if (path) {
            const fpParams = {
                pathTransitions: createTransitions(path),
                tweener: params.tweener,
                charControl: params.charController,
                tileSize: params.tileWidth,
                speed: params.speed,
                cb: params.cb,
            };
            timeline = moveOnPath(fpParams);
        }
        else {
            //TODO handle no path
            timeline = 'NO_PATH';
        }
    });
    params.finder.calculate();
    if (params.tempObsX != undefined && params.tempObsY != undefined) {
        params.finder.stopAvoidingAdditionalPoint(params.tempObsX, params.tempObsY);
    }
    return timeline;
}
function createTransitions(path) {
    let transitions = [];
    for (let i = 1; i < path.length; i++) {
        const xDiff = ensure(path[i].x) - ensure(path[i - 1].x);
        const yDiff = ensure(path[i].y) - ensure(path[i - 1].y);
        transitions.push({ x: xDiff, y: yDiff });
    }
    return transitions;
}
function getTileMoveTween(pathTransition, char, tileSize, speed) {
    //accelration is the bodies change in velocity in pixels per second squared
    // velocity is pixels per second
    // we need to move exactly 8 pixels
    // ok but sometimes we are at top speed and sometimes not
    // 16
    console.log(((tileSize) / speed) * 1000);
    return {
        targets: [],
        paused: false,
        onStart: (tween, targets, param) => {
            const state = getStateFromTransition(pathTransition);
            console.log(state);
            char.move(Dir[state]);
        },
        duration: ((tileSize) / speed) * 1000,
        ease: 'Stepped'
    };
}
//TODO everything should use dir enum...
function getStateFromTransition(pathTransition) {
    if (pathTransition.x === 1 && pathTransition.y === 0) {
        return Dir.east;
        // return 'east';
    }
    else if (pathTransition.x === -1 && pathTransition.y === 0) {
        return Dir.west;
        // return 'west';
    }
    else if (pathTransition.x === 0 && pathTransition.y === 1) {
        return Dir.south;
        // return 'south';
    }
    else if (pathTransition.x === 0 && pathTransition.y === -1) {
        return Dir.north;
        // return 'north';
    }
    else {
        return Dir.north;
        // return 'north';
    }
}
//# sourceMappingURL=moveToTile.js.map