import { IMapObject } from 'components/MapObject';
import { ILevelConfig } from 'ILevelConfig';
import { AssuredVec2 } from 'joegameTypes';
import 'phaser'
import TweetConvo from './components/TweetConvo';
import { ICharacter } from './ICharacter';
import { ILevelComponents } from './ILevel';

/*
 * These are all static methods, should each be independent, with little side effects.
 */
export default interface IjoegameFacade {
    /** * */
    initGame(baseURL: string): Promise<Phaser.Game>
    loadAssets(game: Phaser.Game, mapjsonpath: string): Promise<Phaser.Game>
    loadMapJSON(game: Phaser.Game, path: string): Promise<Phaser.Game>
    loadConvoManifestJSON(game: Phaser.Game): Promise<Phaser.Game>
    createAnims(game: Phaser.Game, mapjsonpath: string): void
    // loadMenu(key:string): IMenuScene
    runLevelScene(game: Phaser.Game, key: string): ILevelComponents

    addAllNPCsFromLayer(level: ILevelComponents, layer: string): void
    addAllTweetConvosFromLayer(level: ILevelComponents, layer: string): Promise<TweetConvo[] | undefined>
    addAllObjectsFromLayer(level: ILevelComponents, layer: string, xOffset?: number, yOffset?: number): IMapObject[]
    addAllPlatformsFromLayer(level: ILevelComponents, layer: string): void
    addPlayerToLevel(level: ILevelComponents, x: number, y: number, char?: string): ICharacter
    createLevelPhysics(level: ILevelComponents): void
    createDepthMap(game: Phaser.Game, mapjsonpath: string): void
    runCinematicNode(level: ILevelComponents, node: string): void
    createTweetConvo(level: ILevelComponents, tx: number, ty: number, charGroup?: string, convoID?: string): void

    createLevel(jsonpath: string, game: Phaser.Game, plyr: AssuredVec2, lvlConfig?: ILevelConfig ): Promise<ILevelComponents>
}
