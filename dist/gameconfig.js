"use strict";

var _interopRequireWildcard = require("@babel/runtime/helpers/interopRequireWildcard");

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.default = createJoegameConfig;

var Phaser = _interopRequireWildcard(require("phaser"));

/**
 * This returns a full config you can pass into `new Phaser.Game(config)`
 * */
// TODO type for resolver
function createJoegameConfig(gdata, res) {
  return {
    type: Phaser.WEBGL,
    render: {
      pixelArt: true
    },
    scale: {
      mode: Phaser.Scale.FIT,
      autoCenter: Phaser.Scale.CENTER_BOTH,
      width: 800,
      height: 600
    },
    parent: 'frame',
    dom: {
      createContainer: true
    },
    physics: {
      default: 'arcade',
      arcade: {
        gravity: {
          y: 0
        },
        debug: false
      }
    },
    scene: {
      preload: function preload() {
        // TODO properly ignore this in some typescript way
        var scenee = this;
        scenee.load.json('gdata', gdata); // rawmap
        // this.load.json(getMapKeyNameRaw(data.mapjson),data.mapjson)
      },
      create: function create() {
        // loadLevel(this, data)
        var scenee = this;
        res(scenee.game);
      },
      key: 'GameInitScene'
    }
  };
}
//# sourceMappingURL=gameconfig.js.map