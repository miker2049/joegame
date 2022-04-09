import { IMapObject } from 'components/MapObject';
import { ILevelConfig } from 'ILevelConfig';
import { AssuredVec2 } from 'joegameTypes';
import 'phaser'
import MIDIPlayer from 'timidity-wasm';
import TweetConvo from './components/TweetConvo';
import { ICharacter } from './ICharacter';
import { ILevelComponents } from './ILevel';

export default interface IjoegameFacade {
    /** * */
    initGame(baseURL: string, config: Phaser.Types.Core.GameConfig): Promise<Phaser.Game>
    loadAssets(game: Phaser.Game, config: ILevelConfig): Promise<Phaser.Game>
    loadMapJSON(game: Phaser.Game, path: string): Promise<Phaser.Game>
    loadConvoManifestJSON(game: Phaser.Game): Promise<Phaser.Game>
    createAnims(game: Phaser.Game, mapjsonpath: string): void
    // loadMenu(key:string): IMenuScene
    runLevelScene(game: Phaser.Game, config: ILevelConfig): ILevelComponents

    addAllNPCsFromLayer(level: ILevelComponents, layer: string): void
    addAllLightsFromLayer(level: ILevelComponents, layer: string): void
    addAllTweetConvosFromLayer(level: ILevelComponents, layer: string): Promise<TweetConvo[] | undefined>
    addAllObjectsFromLayer(level: ILevelComponents, layer: string, xOffset?: number, yOffset?: number): IMapObject[]
    addAllPlatformsFromLayer(level: ILevelComponents, layer: string): void
    addPlayerToLevel(level: ILevelComponents, x: number, y: number, char?: string): ICharacter
    createLevelPhysics(level: ILevelComponents): void
    createDepthMap(game: Phaser.Game, mapjsonpath: string): void
    runCinematicNode(level: ILevelComponents, node: string, data: any): Promise<void>
    createTweetConvo(level: ILevelComponents, tx: number, ty: number, charGroup?: string, convoID?: string): void

    loadMIDIFile(path: string, context?: AudioContext): Promise<MIDIPlayer>

    createLevel(game: Phaser.Game, config: ILevelConfig): Promise<ILevelComponents>
}
