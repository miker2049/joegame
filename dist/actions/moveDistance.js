import { Dir, Axis } from '../joegameTypes';
const THRESHOLD = 0.25;
const DELAY = 2;
export default function (params) {
    return new Promise((res, rej) => {
        let axis;
        let dest;
        switch (params.dir) {
            case Dir.north:
                axis = Axis.yaxis;
                dest = params.gobject.y - params.distance;
                break;
            case Dir.south:
                axis = Axis.yaxis;
                dest = params.gobject.y + params.distance;
                break;
            case Dir.east:
                dest = params.gobject.x + params.distance;
                axis = Axis.xaxis;
                break;
            case Dir.west:
                dest = params.gobject.x - params.distance;
                axis = Axis.xaxis;
                break;
        }
        let timeRun = 0;
        params.gobject.move(params.dir);
        setTimeout(() => {
            res(undefined);
        }, (params.distance / params.gobject.speed) * 1000);
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
    });
}
//# sourceMappingURL=moveDistance.js.map