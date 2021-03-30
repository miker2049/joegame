"use strict";

var _interopRequireDefault = require("@babel/runtime/helpers/interopRequireDefault");

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.default = _default;

var _touchToDir = _interopRequireDefault(require("../utils/touchToDir"));

function _default(level) {
  level.scene.physics.world.addCollider(level.npcs, level.npcs, function (npc1, npc2) {
    level.machineRegisty.sendTo(npc1.name + '_machine', {
      type: 'BUMP',
      sprite: npc2
    });
    level.machineRegisty.sendTo(npc2.name + '_machine', {
      type: 'BUMP',
      sprite: npc1
    });
  });

  if (level.player) {
    level.scene.physics.world.addCollider(level.player, level.npcs, function (npc1, npc2) {
      level.machineRegisty.sendTo('player_machine', {
        type: 'BUMP',
        dir: (0, _touchToDir.default)(npc2.body.touching)
      });
      level.machineRegisty.sendTo(npc2.name + '_machine', {
        type: 'BUMP',
        sprite: npc1
      });
    });
  }

  level.map.layers.forEach(function (l) {
    level.scene.physics.world.addCollider(l.tilemapLayer, level.npcs, function (npc1, npc2) {
      level.machineRegisty.sendTo(npc1.name + '_machine', {
        type: 'BUMP',
        sprite: npc2
      });
      level.machineRegisty.sendTo(npc2.name + '_machine', {
        type: 'BUMP',
        sprite: npc1
      });
    });

    if (level.player) {
      level.scene.physics.world.addCollider(l.tilemapLayer, level.player, function (npc1, npc2) {
        level.machineRegisty.sendTo(npc1.name + '_machine', {
          type: 'BUMP',
          sprite: npc2
        });
        level.machineRegisty.sendTo('player_machine', {
          type: 'BUMP',
          sprite: npc1
        });
      });
    }
  });
}