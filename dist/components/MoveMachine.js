import { Machine, assign, sendParent } from 'xstate';
import 'phaser';
import getPath from '../utils/getPath';
import getDirFromTransition from '../utils/getDirFromTransition';
import moveDistance from '../actions/moveDistance';
import getTileFromPoint from '../utils/getTileFromPoint';
const createPathmoveMachine = (name) => Machine({
    id: `${name}s_pathmover`,
    initial: 'gettingpath',
    states: {
        gettingpath: {
            invoke: {
                src: (context, event) => {
                    const charPlace = context.char.align();
                    return getPath({
                        x: charPlace.x,
                        y: charPlace.y,
                        dx: context.destTile.x,
                        dy: context.destTile.y,
                        tempObsX: context.tempObsTile.x,
                        tempObsY: context.tempObsTile.y,
                        finder: context.finder
                    });
                },
                onDone: {
                    actions: assign({
                        path: (context, event) => event.data,
                    }),
                    target: 'movingOnPath'
                }
            }
        },
        movingOnPath: {
            invoke: {
                src: (context) => {
                    if (context.path.length > 0) {
                        const direction = getDirFromTransition(context.path[0]);
                        return moveDistance({
                            gobject: context.char,
                            dir: direction,
                            distance: context.tileSize,
                            stop: false
                        });
                    }
                    else {
                        console.log('no path!');
                    }
                },
                onDone: [
                    {
                        target: 'movingOnPath',
                        actions: assign({
                            path: (context) => {
                                // console.log(context.path);
                                if (context.path.length > 1) {
                                    return context.path.slice(1);
                                }
                                else {
                                    return [];
                                }
                            }
                        }),
                        cond: (context) => context.path.length > 1
                    },
                    { target: 'reachedDestination' }
                ]
            }
        },
        reachedDestination: {
            type: 'final'
        }
    }
});
export const MoveMachineConfig = (character, tileSize, finder) => {
    return {
        key: `${character.name}_movemachine`,
        initial: 'still',
        entry: ['setGroundVel'],
        context: {
            char: character,
            tileSize: tileSize,
            finder: finder,
        },
        on: {
            TRANSPORT: {
                target: 'still',
                actions: 'transport'
            }
        },
        states: {
            still: {
                entry: ['stillAction'],
                on: {
                    MOVE: ['moving'],
                    MOVE_ON_PATH: { target: "onPath" },
                    DASH: [
                        { target: 'dashing', cond: 'hasCharge' },
                        { actions: 'noChargeAction' },
                    ],
                    STOP: { target: 'still', actions: 'setGroundVel' },
                    PLATFORM_CHANGE: { target: 'still', actions: 'setGroundVel' },
                    BUMP: { target: 'still', actions: 'jumpBack' }
                }
            },
            onPath: {
                invoke: {
                    src: (context) => createPathmoveMachine(context.char.name),
                    data: {
                        tileSize: (context) => context.tileSize,
                        char: (context) => context.char,
                        destTile: (context, event) => {
                            const tile = getTileFromPoint({ x: event.point.x, y: event.point.y }, context.tileSize);
                            return { x: tile.x, y: tile.y };
                        },
                        tempObsTile: (_context, event) => event.tempObs || { x: -9999, y: -9999 },
                        finder: (context) => context.finder,
                        path: []
                    },
                    onDone: { target: 'still', actions: sendParent('DESTINATION_REACHED') }
                },
                on: {
                    DESTINATION_REACHED: 'still',
                    BUMP: { actions: 'jumpBack', target: 'still' },
                    MOVE_ON_PATH: { target: 'onPath' },
                    NO_PATH: { target: 'still', actions: 'jumpUp' },
                    MOVE: ['moving'],
                    DASH: [
                        { target: 'dashing', cond: 'hasCharge' },
                        { actions: 'noChargeAction' },
                    ],
                }
            },
            moving: {
                entry: ['setLastDir', 'movingAction'],
                on: {
                    MOVE: 'moving',
                    STOP: { target: 'still', actions: 'setGroundVel' },
                    BUMP: { actions: 'jumpBack', target: 'moving' },
                    LEAVE_PLATFORM: { target: 'moving', actions: 'setGroundVel' },
                    PLATFORM_CHANGE: { target: 'moving', actions: 'setGroundVel' }
                }
            },
            dashing: {
                entry: ['dashAction', 'minusCharge'],
                on: {
                    DASH: [
                        { target: 'dashing', cond: 'hasCharge' },
                        { actions: 'noChargeAction' },
                    ],
                },
                after: {
                    DASH_DELAY: { target: 'still', actions: 'setGroundVel' },
                }
            },
            dead: {
                type: 'final'
            }
        },
    };
};
const MoveMachineOptions = () => {
    return {
        actions: {
            movingAction: (context, event) => {
                const dir = event.dir ? event.dir : context.char.facing;
                context.char.move(dir);
            },
            dashAction: (context, event) => context.char.dash(event.dir),
            minusCharge: (context) => context.char.minusCharge(),
            jumpUp: (context) => context.char.jumpUp(),
            jumpBack: (context, event) => context.char.jumpBack(event.dir),
            setGroundVel: (context) => { context.char.changeGroundVel({ x: 0, y: 0 }); },
            stillAction: (context) => context.char.stop(),
            transport: (context, event) => context.char.transport(event.point.x, event.point.y)
        },
        guards: {
            hasCharge: (context, _event) => context.char.charge > 0 ? true : false,
        },
        activities: {},
        services: {},
        delays: {
            DASH_DELAY: (context, event) => {
                return (context.char.dashVel / context.char.dashDrag) * 1000;
            },
        }
    };
};
export function createMoveMachine(char, tileSize, finder) {
    const config = MoveMachineConfig(char, tileSize, finder);
    const options = MoveMachineOptions();
    return Machine(config, options);
}
//# sourceMappingURL=MoveMachine.js.map