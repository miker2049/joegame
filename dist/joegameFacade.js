import createGameConfig from './gameconfig';
import IjoegameFacade from './IjoegameFacade';
import loadMapJSON from './utils/loadMapJSON';
import loadMapAssets from './utils/loadMapAssets';
import createAnims from './utils/createAnims';
import createBaseLevel from './factories/createBaseLevel';
import addAllNPCsFromLayer from './actions/addAllNPCsFromLayer';
import addAllTweetConvosFromLayer from './actions/addAllTweetConvosFromLayer';
import addAllObjectsFromLayer from './actions/addAllObjectsFromLayer';
import addAllPlatformsFromLayer from './actions/addAllPlatformsFromLayer';
import addPlayerToLevel from './actions/addPlayerToLevel';
import createLevelPhysics from './factories/createLevelPhysics';
import createDepthMap from './utils/createDepthMap';
import runCinematicNode from './actions/runCinematicNode';
import createTweetConvo from './factories/createTweetConvo';
import loadConvoManifestJSON from './utils/loadConvoManifestJSON';
export default class joegameFacade extends IjoegameFacade {
    constructor() {
        super(...arguments);
        this.createAnims = createAnims;
        this.runLevelScene = createBaseLevel;
        this.addAllNPCsFromLayer = addAllNPCsFromLayer;
        this.addAllTweetConvosFromLayer = addAllTweetConvosFromLayer;
        this.addAllObjectsFromLayer = addAllObjectsFromLayer;
        this.addAllPlatformsFromLayer = addAllPlatformsFromLayer;
        this.addPlayerToLevel = addPlayerToLevel;
        this.createLevelPhysics = createLevelPhysics;
        this.createDepthMap = createDepthMap;
        this.runCinematicNode = runCinematicNode;
        this.createTweetConvo = createTweetConvo;
    }
    initGame(gdata) {
        console.log('this here hoheee?');
        return new Promise((resolve, reject) => {
            new Phaser.Game(createGameConfig(gdata, resolve));
        });
    }
    loadMapJSON(game, mapjsonpath) {
        return loadMapJSON(game, mapjsonpath);
    }
    loadAssets(game, mapjsonpath) {
        return loadMapAssets(game, mapjsonpath);
    }
    loadConvoManifestJSON(game) {
        return loadConvoManifestJSON(game);
    }
}
//# sourceMappingURL=joegameFacade.js.map