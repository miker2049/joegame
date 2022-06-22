import 'phaser'
import distanceBetweenPoints from 'utils/distanceBetweenPoints'
import {GameObjectInWorld} from '../joegameTypes'
/**
 * @param {Phaser.GameObjects.GameObject} obj
 * @param {Phaser.GameObjects.GameObject} dest
 * @param {number} speed
 * Float up, and to dest, with speed factored
 */
const FLOAT_CHECK_FPS = 12
const BASE_DUR = 1000
export default async function(obj: GameObjectInWorld, dest: GameObjectInWorld, speed: number, jumpAmount?: number){

    await new Promise((res,_rej)=>{
            obj.scene.tweens.add({
                targets: obj,
                y: `-=${jumpAmount ?? 15}`,
                duration: 500/speed,
                ease: 'linear',
                onComplete: (_tween) => {
                    res(undefined)
                }
            })
    })

    await new Promise((res, rej) => {
        const updatefunc=()=>{
            if (obj.scene.physics.overlap(obj,dest)) {
                obj.scene.events.removeListener('update',updatefunc,obj)
                res(undefined)
            } else {
                // obj.scene.physics.accelerateToObject(obj,dest,1000,1000,1000)
                const dist=distanceBetweenPoints({
                    x: obj.x,
                    y: obj.y,
                },{
                    x:dest.body.center.x,
                    y:dest.body.center.y
                })
                if (dist<20) {
                    obj.scale = dist/20
                }
                obj.scene.physics.moveTo(obj, dest.body.center.x, dest.body.center.y,300)
            }
        }
        obj.scene.events.on('update',updatefunc,obj)
        // obj.scene.tweens.addCounter({
        //     from: 0,
        //     to: (FLOAT_CHECK_FPS*BASE_DUR)/60,
        //     duration: 250*initDistance,
        //     ease: 'linear',
        //     onUpdate: (tween)=>{
        //         let val = Math.floor(tween.getValue())
        //         if(Phaser.Math.Distance.Between(obj.x,obj.y,dest.x,dest.y)<5){
        //             tween.remove()
        //             res(undefined)
        //         }
        //         if(val>i){
        //             i = val
        //             console.log("tween up", val, obj.x,obj.y,dest.x,dest.y)
        //             obj.scene.physics.accelerateToObject(obj,dest,150)
        //         }
        //     },
        //     onComplete: (_tween) => {
        //         res(undefined)
        //     }
        // })
    })

}
