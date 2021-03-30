"use strict";

var _interopRequireDefault = require("@babel/runtime/helpers/interopRequireDefault");

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.createNPCMachine = createNPCMachine;

var _xstate = require("xstate");

var _joegameTypes = require("../joegameTypes");

var _MoveMachine = require("./MoveMachine");

var _defaults = _interopRequireDefault(require("../defaults"));

var _getTileFromBody = _interopRequireDefault(require("../utils/getTileFromBody"));

var machineConfig = function machineConfig(name) {
  return {
    id: 'NPCMachine' + name,
    initial: 'idle',
    entry: 'spawnMoveMachine',
    on: {
      // AUTO_OFF:{actions: assign({auto: ()=>false})},
      AUTO_OFF: {
        actions: (0, _xstate.assign)({
          auto: function auto(context) {
            return false;
          }
        }),
        target: "idle"
      },
      AUTO_ON: {
        actions: (0, _xstate.assign)({
          auto: function auto(context) {
            return true;
          }
        }),
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
            actions: function actions() {
              console.log('you say something');
            }
          },
          BUMP: {
            target: 'stoppedAndTurned'
          }
        },
        entry: ['moveToInterest']
      },
      stoppedAndTurned: {
        entry: ['reactToCollider', 'exclaim', 'jumpBack'],
        after: {
          2000: [{
            target: 'going'
          }]
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
            actions: function actions() {
              console.log('you say something');
            }
          },
          BUMP: {
            target: 'stoppedAndTurned'
          }
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
}; // export default Machine(machineConfig,machineOptions)


var opts = {
  actions: {
    chooseInterest: (0, _xstate.assign)({
      interestCounter: function interestCounter(context) {
        return (context.interestCounter + 1) % context.interests.length;
      },
      currentDestination: function currentDestination(context) {
        var curr = context.interests[context.interestCounter];
        return {
          x: curr.x,
          y: curr.y
        };
      }
    }),
    setInterest: (0, _xstate.assign)({
      currentDestination: function currentDestination(_context, event) {
        return {
          x: event.x,
          y: event.y
        };
      },
      tmpFinalFacing: function tmpFinalFacing(_context, event) {
        if (event.finalFacing != undefined) {
          return event.finalFacing;
        } else {
          return undefined;
        }
      }
    }),
    unsetFinalFacing: (0, _xstate.assign)({
      tmpFinalFacing: function tmpFinalFacing(_context) {
        return undefined;
      }
    }),
    startDialogue: function startDialogue(_context) {},
    reactToCollider: (0, _xstate.assign)({
      additionalAvoid: function additionalAvoid(context, event) {
        return (0, _getTileFromBody.default)(event.sprite, context.tileSize);
      }
    }),
    jumpBack: (0, _xstate.send)(function (context, event) {
      var collider = event.sprite;
      console.log(event);

      if (collider.y > context.character.y) {
        return {
          type: 'BUMP',
          dir: _joegameTypes.Dir.south
        };
      } else if (collider.y < context.character.y) {
        return {
          type: 'BUMP',
          dir: _joegameTypes.Dir.north
        };
      } else if (collider.x < context.character.x) {
        return {
          type: 'BUMP',
          dir: _joegameTypes.Dir.west
        };
      } else if (collider.x > context.character.x) {
        return {
          type: 'BUMP',
          dir: _joegameTypes.Dir.east
        };
      } else {
        return {
          type: 'BUMP',
          dir: _joegameTypes.Dir.east
        };
      }
    }, {
      to: function to(context) {
        return context.moveMachineRef;
      }
    }),
    exclaim: function exclaim(_context) {},
    announceReached: function announceReached(_context) {},
    moveToInterest: (0, _xstate.send)(function (context) {
      return {
        type: 'MOVE_ON_PATH',
        point: {
          x: context.currentDestination.x,
          y: context.currentDestination.y
        },
        tempObs: {
          x: context.additionalAvoid.x,
          y: context.additionalAvoid.y
        }
      };
    }, {
      to: function to(context) {
        return context.moveMachineRef;
      }
    }),
    spawnMoveMachine: (0, _xstate.assign)({
      moveMachineRef: function moveMachineRef(context) {
        return (0, _xstate.spawn)((0, _MoveMachine.createMoveMachine)(context.character, context.tileSize, context.finder));
      }
    }) // duration:2000,

  },
  guards: {
    isAuto: function isAuto(context) {
      return context.auto;
    }
  },
  activities: {},
  services: {},
  delays: {}
};

function createNPCMachine(char, tileSize, finder, interests) {
  return (0, _xstate.Machine)(machineConfig(char.name), opts).withContext({
    character: char,
    tileSize: tileSize,
    finder: finder,
    interests: interests,
    currentDestination: interests[1],
    patience: _defaults.default.patience,
    auto: true,
    tmpFinalFacing: undefined,
    interestCounter: 0,
    additionalAvoid: {
      x: 0,
      y: 0
    }
  });
}