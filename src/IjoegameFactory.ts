import TiledRawJson from '../typings/TiledRawJson'
import { IWikiData } from './utils/parseWikiData';
interface IjoegameFactory {
    loadAssets(mapjson: TiledRawJson, wikidata: IWikiData)
}
