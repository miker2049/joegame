import { Dir, Axis } from '../joegameTypes'

export interface ImoveDistanceObject {
    x: number
    y: number
    speed: number
    move(dir: Dir): void
    stop(face?: Dir): void
    align(): Phaser.Types.Math.Vector2Like

}

interface ImoveDistanceParams {
    gobject: ImoveDistanceObject
    dir: Dir
    distance: number
    stop?: boolean
}

export default function(params: ImoveDistanceParams): Promise<any> {
    return new Promise((res, rej) => {
        let axis: Axis
        let dest: number
        switch (params.dir) {
            case Dir.north:
                axis = Axis.yaxis
                dest = params.gobject.y - params.distance
                break
            case Dir.south:
                axis = Axis.yaxis
                dest = params.gobject.y + params.distance
                break
            case Dir.east:
                dest = params.gobject.x + params.distance
                axis = Axis.xaxis
                break
            case Dir.west:
                dest = params.gobject.x - params.distance
                axis = Axis.xaxis
                break
        }
        let timeRun = 0
        params.gobject.move(params.dir)
        setTimeout(() => {
            res(undefined)
        }, (params.distance / params.gobject.speed) * 1000)
        // var inter = setInterval(() => {
        //     const target = axis === Axis.yaxis ? params.gobject.y : params.gobject.x
        //     if (Math.abs(dest - target) < THRESHOLD || target > dest) {
        //         clearInterval(inter)
        //         if (params.stop === true) params.gobject.stop()
        //         res(undefined)
        //     }
        //     if (((params.distance / params.gobject.speed) * 1000) / (timeRun * DELAY) < 1 / 2) {
        //         clearInterval(inter)
        //     }
        //     timeRun++
        // }, DELAY)
        // params.moveMachine.onTransition(state=> state.value != 'onPath' ? clearInterval(inter) : undefined )
    })
}
