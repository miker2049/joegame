import IjoegameFacade from './IjoegameFacade';
import createAnims from './utils/createAnims';
import createBaseLevel from './factories/createBaseLevel';
import { IWikiData } from './utils/parseWikiData';
import addAllNPCsFromLayer from './actions/addAllNPCsFromLayer';
import addAllTweetConvosFromLayer from './actions/addAllTweetConvosFromLayer';
import addAllObjectsFromLayer from './actions/addAllObjectsFromLayer';
import addAllPlatformsFromLayer from './actions/addAllPlatformsFromLayer';
import addPlayerToLevel from './actions/addPlayerToLevel';
import createLevelPhysics from './factories/createLevelPhysics';
import createDepthMap from './utils/createDepthMap';
import runCinematicNode from './actions/runCinematicNode';
import createTweetConvo from './factories/createTweetConvo';
export default class joegameFacade extends IjoegameFacade {
    initGame(gdata: IWikiData, convo: string | any[]): Promise<Phaser.Game>;
    loadMapJSON(game: Phaser.Game, mapjsonpath: string): Promise<Phaser.Game>;
    loadAssets(game: Phaser.Game, mapjsonpath: string): Promise<Phaser.Game>;
    createAnims: typeof createAnims;
    runLevelScene: typeof createBaseLevel;
    addAllNPCsFromLayer: typeof addAllNPCsFromLayer;
    addAllTweetConvosFromLayer: typeof addAllTweetConvosFromLayer;
    addAllObjectsFromLayer: typeof addAllObjectsFromLayer;
    addAllPlatformsFromLayer: typeof addAllPlatformsFromLayer;
    addPlayerToLevel: typeof addPlayerToLevel;
    createLevelPhysics: typeof createLevelPhysics;
    createDepthMap: typeof createDepthMap;
    runCinematicNode: typeof runCinematicNode;
    createTweetConvo: typeof createTweetConvo;
}
