import 'phaser';
import TweetConvo from './components/TweetConvo';
import { ICharacter } from './ICharacter';
import { ILevelComponents } from './ILevel';
import { IWikiData } from './utils/parseWikiData';
export default abstract class IjoegameFacade {
    /** * */
    abstract initGame(gdata: IWikiData, convo: string | any[]): Promise<Phaser.Game>;
    abstract loadAssets(game: Phaser.Game, mapjsonpath: string): Promise<Phaser.Game>;
    abstract loadMapJSON(game: Phaser.Game, path: string): Promise<Phaser.Game>;
    abstract createAnims(game: Phaser.Game): void;
    abstract runLevelScene(game: Phaser.Game, key: string): ILevelComponents;
    abstract addAllNPCsFromLayer(level: ILevelComponents, layer: string): void;
    abstract addAllTweetConvosFromLayer(level: ILevelComponents, layer: string): Promise<TweetConvo[] | undefined>;
    abstract addAllObjectsFromLayer(level: ILevelComponents, layer: string, xOffset?: number, yOffset?: number): Phaser.GameObjects.Image[];
    abstract addAllPlatformsFromLayer(level: ILevelComponents, layer: string): void;
    abstract addPlayerToLevel(level: ILevelComponents, x: number, y: number, char?: string): ICharacter;
    abstract createLevelPhysics(level: ILevelComponents): void;
    abstract createDepthMap(game: Phaser.Game, mapjsonpath: string): void;
    abstract runCinematicNode(level: ILevelComponents, node: string): any;
    abstract createTweetConvo(level: ILevelComponents, tx: number, ty: number, charGroup?: string, convoID?: string): any;
}
