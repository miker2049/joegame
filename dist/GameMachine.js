import 'phaser';
const GameMachineOptions = {
    actions: {},
    services: {
        createGame: (context, _event) => context.facade.initGame(),
        loadMapJson: (context, _event) => context.facade.loadMapJSON(context.game, context.currentMap),
        loadMapAssets: (context, _event) => context.facade.loadAssets(context.game, context.currentMap)
    },
    guards: {},
    activities: {},
    delays: {}
};
//# sourceMappingURL=GameMachine.js.map