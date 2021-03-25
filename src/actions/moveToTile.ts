import 'phaser'
import {Dir} from '../joegameTypes'
import {ICharacterControl} from '../ICharacter'
import { IPathfinder } from '../ILevel';
import ensure from '../utils/ensure';

interface moveOnPathParams {
    pathTransitions: Phaser.Types.Math.Vector2Like[]
    tweener: Phaser.Tweens.TweenManager
    charControl: ICharPathMoveControl
    finalFacing?: Dir
    tileSize: number
    speed: number
    cb: (s: string)=>void
}

interface ICharPathMoveControl {
    face(dir: Dir): void
    move(dir: Dir): void
    stop(): void
    align(): void
    charBody: Phaser.Physics.Arcade.Body
}
function moveOnPath(params: moveOnPathParams): Phaser.Tweens.Timeline{
    // this.char.body.allowDrag = false;
    let tweens: Array<Phaser.Types.Tweens.TweenBuilderConfig> = [] ;
    for(let transition of params.pathTransitions){
        tweens.push(getTileMoveTween(transition,params.charControl,params.tileSize, params.speed) as Phaser.Tweens.Tween)
    }
    tweens.push({
        targets: [params.charControl.charBody],
        duration: 100,
        onComplete: ()=>{
            if (params.finalFacing != undefined){
                params.charControl.face(params.finalFacing)
            }
            params.charControl.stop()
            params.cb
        },
    })
    return params.tweener.timeline({tweens: tweens})
}

export interface moveToTileParams {
    x: number
    y: number
    dx: number
    dy: number
    tempObsX?: number
    tempObsY?: number
    tileWidth: number
    tileHeight: number
    finalFacing?: Dir
    charController: ICharPathMoveControl
    finder: IPathfinder
    tweener: Phaser.Tweens.TweenManager
    speed: number
    cb: (s: string)=>void
}
export function moveToTile(params: moveToTileParams): Phaser.Tweens.Timeline | string {
    let timeline: Phaser.Tweens.Timeline | string = 'NO_PATH'
    // destroy the current tween timeline that is running
    // this.char.destTimeline.destroy();
    //This janky movement doesnt seem to matter so much when you start moving and animating, makes sure the distance calculations are correct too
    params.charController.align()
    // get the players current tile
    // here the callback first takes the path and makes it in to a bunch of (-1/0/1,-1/0/1) type transitions, then puts that into the path function
    if(params.tempObsX != undefined && params.tempObsY != undefined){
        params.finder.avoidAdditionalPoint(params.tempObsX,params.tempObsY);
    }
    params.finder.findPath(params.x,params.y,params.dx,params.dy,(path)=>{
        console.log(path)
        if (path) {
            const fpParams: moveOnPathParams = {
                pathTransitions: createTransitions(path),
                tweener: params.tweener,
                charControl: params.charController,
                tileSize: params.tileWidth,
                speed: params.speed,
                cb: params.cb,
            }
            timeline = moveOnPath(fpParams)
        } else {
            //TODO handle no path
            timeline = 'NO_PATH'
        }
    });
    params.finder.calculate();
    if(params.tempObsX != undefined && params.tempObsY != undefined){
        params.finder.stopAvoidingAdditionalPoint(params.tempObsX,params.tempObsY);
    }
    return timeline
}

function createTransitions(path:Phaser.Types.Math.Vector2Like[]):Phaser.Types.Math.Vector2Like[] {
    let transitions: Phaser.Types.Math.Vector2Like[] = [];
    for (let i =1; i<path.length;i++) {
        const xDiff = ensure(path[i].x) - ensure(path[i-1].x);
        const yDiff = ensure(path[i].y) - ensure(path[i-1].y);
        transitions.push({x: xDiff, y: yDiff})

    }
    return transitions;
}

function getTileMoveTween(pathTransition: Phaser.Types.Math.Vector2Like, char: ICharPathMoveControl, tileSize: number, speed: number): Phaser.Types.Tweens.TweenBuilderConfig {
    //accelration is the bodies change in velocity in pixels per second squared
    // velocity is pixels per second
    // we need to move exactly 8 pixels
    // ok but sometimes we are at top speed and sometimes not
    // 16
    console.log(( (tileSize)/speed ) * 1000)
    return {
        targets: [],
        paused: false,
        onStart: (tween, targets, param)=>{
            const state = getStateFromTransition(pathTransition)
            console.log(state)
            char.move(Dir[state])
        },
        duration: ( (tileSize)/speed ) * 1000, //have to disallow drag here for this
        ease: 'Stepped'
    }
}

//TODO everything should use dir enum...
function getStateFromTransition(pathTransition: Phaser.Types.Math.Vector2Like){
    if (pathTransition.x === 1 && pathTransition.y === 0 ){
        return Dir.east;
        // return 'east';
    } else if (pathTransition.x === -1 && pathTransition.y === 0 ){
        return Dir.west;
        // return 'west';
    } else if (pathTransition.x === 0 && pathTransition.y === 1 ){
        return Dir.south;
        // return 'south';
    } else if (pathTransition.x === 0 && pathTransition.y === -1 ){
        return Dir.north;
        // return 'north';
    } else {
        return Dir.north;
        // return 'north';
    }
}

