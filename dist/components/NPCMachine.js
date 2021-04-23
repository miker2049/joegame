import { Machine, assign, send, spawn } from 'xstate';
import { Dir } from '../joegameTypes';
import { createMoveMachine } from './MoveMachine';
import defaults from '../defaults';
import getTileFromBody from '../utils/getTileFromBody';
const machineConfig = (name) => {
    return {
        id: 'NPCMachine' + name,
        initial: 'idle',
        entry: 'spawnMoveMachine',
        on: {
            // AUTO_OFF:{actions: assign({auto: ()=>false})},
            AUTO_OFF: {
                actions: assign({ auto: (context) => { return false; } }),
                target: "idle"
            },
            AUTO_ON: {
                actions: assign({ auto: (context) => { return true; } }),
                target: "idle"
            }
        },
        states: {
            going: {
                on: {
                    DESTINATION_REACHED: {
                        target: 'idle',
                        actions: ['unsetFinalFacing', 'announceReached']
                    },
                    SPEAK_THOUGHT: {
                        actions: () => { console.log('you say something'); }
                    },
                    BUMP: {
                        target: 'stoppedAndTurned',
                    },
                },
                entry: ['moveToInterest']
            },
            stoppedAndTurned: {
                entry: ['reactToCollider', 'exclaim', 'jumpBack'],
                after: {
                    2000: [{ target: 'going' }]
                },
                on: {
                    TALK_TO: {
                        target: 'conversing',
                        actions: 'startDialogue'
                    }
                }
            },
            idle: {
                on: {
                    MOVE_THOUGHT: {
                        target: 'going',
                        actions: ['setInterest']
                    },
                    SPEAK_THOUGHT: {
                        actions: () => { console.log('you say something'); }
                    },
                    BUMP: {
                        target: 'stoppedAndTurned',
                    },
                },
                after: {
                    2000: {
                        target: 'going',
                        cond: 'isAuto',
                        actions: ['chooseInterest']
                    }
                }
            },
            conversing: {
                on: {
                    CONVERSATION_DONE: 'idle'
                }
            }
        }
    };
};
// export default Machine(machineConfig,machineOptions)
const opts = {
    actions: {
        chooseInterest: assign({
            interestCounter: (context) => { return (context.interestCounter + 1) % context.interests.length; },
            currentDestination: (context) => {
                const curr = context.interests[context.interestCounter];
                return { x: curr.x, y: curr.y };
            }
        }),
        setInterest: assign({
            currentDestination: (_context, event) => { return { x: event.x, y: event.y }; },
            tmpFinalFacing: (_context, event) => { if (event.finalFacing != undefined) {
                return event.finalFacing;
            }
            else {
                return undefined;
            } }
        }),
        unsetFinalFacing: assign({ tmpFinalFacing: (_context) => undefined }),
        startDialogue: (_context) => { },
        reactToCollider: assign({
            additionalAvoid: (context, event) => getTileFromBody(event.sprite, context.tileSize)
        }),
        jumpBack: send((context, event) => {
            const collider = event.sprite;
            console.log(event);
            if (collider.y > context.character.y) {
                return { type: 'BUMP', dir: Dir.south };
            }
            else if (collider.y < context.character.y) {
                return { type: 'BUMP', dir: Dir.north };
            }
            else if (collider.x < context.character.x) {
                return { type: 'BUMP', dir: Dir.west };
            }
            else if (collider.x > context.character.x) {
                return { type: 'BUMP', dir: Dir.east };
            }
            else {
                return { type: 'BUMP', dir: Dir.east };
            }
        }, {
            to: (context) => context.moveMachineRef
        }),
        exclaim: (_context) => { },
        announceReached: (_context) => { },
        moveToInterest: send((context) => {
            return {
                type: 'MOVE_ON_PATH',
                point: { x: context.currentDestination.x, y: context.currentDestination.y },
                tempObs: { x: context.additionalAvoid.x, y: context.additionalAvoid.y }
            };
        }, { to: (context) => context.moveMachineRef }),
        spawnMoveMachine: assign({
            moveMachineRef: (context) => {
                return spawn(createMoveMachine(context.character, context.tileSize, context.finder));
            }
        }),
        // duration:2000,
    },
    guards: {
        isAuto: (context) => context.auto
    },
    activities: {},
    services: {},
    delays: {}
};
export function createNPCMachine(char, tileSize, finder, interests) {
    return Machine(machineConfig(char.name), opts).withContext({
        character: char,
        tileSize: tileSize,
        finder: finder,
        interests: interests,
        currentDestination: interests[1],
        patience: defaults.patience,
        auto: true,
        tmpFinalFacing: undefined,
        interestCounter: 0,
        additionalAvoid: { x: 0, y: 0 }
    });
}
//# sourceMappingURL=NPCMachine.js.map