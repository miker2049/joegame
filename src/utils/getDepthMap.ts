import 'phaser'
import TiledRawJSON from '../types/TiledRawJson';
import { getMapKeyNameRaw } from './getKeyNames'

export default function(game: Phaser.Game, mapjsonpath: string): Map<string, number> {
    const mapjson = game.cache.json.get(getMapKeyNameRaw(mapjsonpath)) as TiledRawJSON
    const layers: [string, number][] = mapjson.layers.map((l, i) => [l.name, i])
    const map = new Map<string, number>(layers)
    game.registry.remove('depthmap')
    game.registry.set('depthmap', map)
    return map
}
