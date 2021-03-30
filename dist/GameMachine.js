"use strict";

require("phaser");

var GameMachineOptions = {
  actions: {},
  services: {
    createGame: function createGame(context, _event) {
      return context.facade.initGame();
    },
    loadMapJson: function loadMapJson(context, _event) {
      return context.facade.loadMapJSON(context.game, context.currentMap);
    },
    loadMapAssets: function loadMapAssets(context, _event) {
      return context.facade.loadAssets(context.game, context.currentMap);
    }
  },
  guards: {},
  activities: {},
  delays: {}
};