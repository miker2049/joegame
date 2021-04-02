"use strict";

var _interopRequireDefault = require("@babel/runtime/helpers/interopRequireDefault");

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.createMoveMachine = createMoveMachine;
exports.MoveMachineConfig = void 0;

var _xstate = require("xstate");

require("phaser");

var _getPath = _interopRequireDefault(require("../utils/getPath"));

var _getDirFromTransition = _interopRequireDefault(require("../utils/getDirFromTransition"));

var _moveDistance = _interopRequireDefault(require("../actions/moveDistance"));

var _getTileFromPoint = _interopRequireDefault(require("../utils/getTileFromPoint"));

var createPathmoveMachine = function createPathmoveMachine(name) {
  return (0, _xstate.Machine)({
    id: "".concat(name, "s_pathmover"),
    initial: 'gettingpath',
    states: {
      gettingpath: {
        invoke: {
          src: function src(context, event) {
            var charPlace = context.char.align();
            return (0, _getPath.default)({
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
            actions: (0, _xstate.assign)({
              path: function path(context, event) {
                return event.data;
              }
            }),
            target: 'movingOnPath'
          }
        }
      },
      movingOnPath: {
        invoke: {
          src: function src(context) {
            if (context.path.length > 0) {
              var direction = (0, _getDirFromTransition.default)(context.path[0]);
              return (0, _moveDistance.default)({
                gobject: context.char,
                dir: direction,
                distance: context.tileSize,
                stop: false
              });
            } else {
              console.log('no path!');
            }
          },
          onDone: [{
            target: 'movingOnPath',
            actions: (0, _xstate.assign)({
              path: function path(context) {
                // console.log(context.path);
                if (context.path.length > 1) {
                  return context.path.slice(1);
                } else {
                  return [];
                }
              }
            }),
            cond: function cond(context) {
              return context.path.length > 1;
            }
          }, {
            target: 'reachedDestination'
          }]
        }
      },
      reachedDestination: {
        type: 'final'
      }
    }
  });
};

var MoveMachineConfig = function MoveMachineConfig(character, tileSize, finder) {
  return {
    key: "".concat(character.name, "_movemachine"),
    initial: 'still',
    entry: ['setGroundVel'],
    context: {
      char: character,
      tileSize: tileSize,
      finder: finder
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
          MOVE_ON_PATH: {
            target: "onPath"
          },
          DASH: [{
            target: 'dashing',
            cond: 'hasCharge'
          }, {
            actions: 'noChargeAction'
          }],
          STOP: {
            target: 'still',
            actions: 'setGroundVel'
          },
          PLATFORM_CHANGE: {
            target: 'still',
            actions: 'setGroundVel'
          },
          BUMP: {
            target: 'still',
            actions: 'jumpBack'
          }
        }
      },
      onPath: {
        invoke: {
          src: function src(context) {
            return createPathmoveMachine(context.char.name);
          },
          data: {
            tileSize: function tileSize(context) {
              return context.tileSize;
            },
            char: function char(context) {
              return context.char;
            },
            destTile: function destTile(context, event) {
              var tile = (0, _getTileFromPoint.default)({
                x: event.point.x,
                y: event.point.y
              }, context.tileSize);
              return {
                x: tile.x,
                y: tile.y
              };
            },
            tempObsTile: function tempObsTile(_context, event) {
              return event.tempObs || {
                x: -9999,
                y: -9999
              };
            },
            finder: function finder(context) {
              return context.finder;
            },
            path: []
          },
          onDone: {
            target: 'still',
            actions: (0, _xstate.sendParent)('DESTINATION_REACHED')
          }
        },
        on: {
          DESTINATION_REACHED: 'still',
          BUMP: {
            actions: 'jumpBack',
            target: 'still'
          },
          MOVE_ON_PATH: {
            target: 'onPath'
          },
          NO_PATH: {
            target: 'still',
            actions: 'jumpUp'
          },
          MOVE: ['moving'],
          DASH: [{
            target: 'dashing',
            cond: 'hasCharge'
          }, {
            actions: 'noChargeAction'
          }]
        }
      },
      moving: {
        entry: ['setLastDir', 'movingAction'],
        on: {
          MOVE: 'moving',
          STOP: {
            target: 'still',
            actions: 'setGroundVel'
          },
          BUMP: {
            actions: 'jumpBack',
            target: 'moving'
          },
          LEAVE_PLATFORM: {
            target: 'moving',
            actions: 'setGroundVel'
          },
          PLATFORM_CHANGE: {
            target: 'moving',
            actions: 'setGroundVel'
          }
        }
      },
      dashing: {
        entry: ['dashAction', 'minusCharge'],
        on: {
          DASH: [{
            target: 'dashing',
            cond: 'hasCharge'
          }, {
            actions: 'noChargeAction'
          }]
        },
        after: {
          DASH_DELAY: {
            target: 'still',
            actions: 'setGroundVel'
          }
        }
      },
      dead: {
        type: 'final'
      }
    }
  };
};

exports.MoveMachineConfig = MoveMachineConfig;

var MoveMachineOptions = function MoveMachineOptions() {
  return {
    actions: {
      movingAction: function movingAction(context, event) {
        var dir = event.dir ? event.dir : context.char.facing;
        context.char.move(dir);
      },
      dashAction: function dashAction(context, event) {
        return context.char.dash(event.dir);
      },
      minusCharge: function minusCharge(context) {
        return context.char.minusCharge();
      },
      jumpUp: function jumpUp(context) {
        return context.char.jumpUp();
      },
      jumpBack: function jumpBack(context, event) {
        return context.char.jumpBack(event.dir);
      },
      setGroundVel: function setGroundVel(context) {
        context.char.changeGroundVel({
          x: 0,
          y: 0
        });
      },
      stillAction: function stillAction(context) {
        return context.char.stop();
      },
      transport: function transport(context, event) {
        return context.char.transport(event.point.x, event.point.y);
      }
    },
    guards: {
      hasCharge: function hasCharge(context, _event) {
        return context.char.charge > 0 ? true : false;
      }
    },
    activities: {},
    services: {},
    delays: {
      DASH_DELAY: function DASH_DELAY(context, event) {
        return context.char.dashVel / context.char.dashDrag * 1000;
      }
    }
  };
};

function createMoveMachine(char, tileSize, finder) {
  var config = MoveMachineConfig(char, tileSize, finder);
  var options = MoveMachineOptions();
  return (0, _xstate.Machine)(config, options);
}
//# sourceMappingURL=MoveMachine.js.map