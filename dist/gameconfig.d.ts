import "phaser";
import { IWikiData } from './utils/parseWikiData';
/**
 * This returns a full config you can pass into `new Phaser.Game(config)`
 * */
export default function createJoegameConfig(gdata: IWikiData | string, convoManifest: any[] | string, res: Function): Phaser.Types.Core.GameConfig;
