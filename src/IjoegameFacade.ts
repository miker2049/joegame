import 'phaser'
import { ILevelComponents } from './ILevel';
export default abstract class IjoegameFacade {
    /** * */
    abstract initGame(): Promise<Phaser.Game>
    abstract loadAssets(game: Phaser.Game, mapjsonpath: string): Promise<Phaser.Game>
    abstract loadMapJSON(game: Phaser.Game, path:string): Promise<Phaser.Game>
    abstract createAnims(game: Phaser.Game): void
    // abstract loadMenu(key:string): IMenuScene
    abstract runLevelScene(game: Phaser.Game, key:string): ILevelComponents
}
