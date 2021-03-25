import 'phaser'
import { IWikiData } from './parseWikiData';
export default function(game: Phaser.Game): IWikiData{ return game.cache.json.get('gdata')}
