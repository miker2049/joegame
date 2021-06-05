import 'phaser'
import TiledRawJSON from '../types/TiledRawJson';
import { getMapKeyNameRaw } from './getKeyNames'

export default function(game: Phaser.Game, mapjsonpath: string): void {
    const mapjson = game.cache.json.get(getMapKeyNameRaw(mapjsonpath)) as TiledRawJSON
    const map = new Map<string, number>(mapjson.layers.map((l, i) => [l.name, i]))
    game.registry.remove('depthmap')
    game.registry.set('depthmap', map)
}
