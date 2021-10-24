import { CanyonSwirl } from 'components/CanyonSwirl'
import MapItem from 'components/MapItem'
import OverlayParticles from 'components/OverlayParticles'
import { ILevelComponents } from 'ILevel'
import { createMapObjectConfig } from 'utils/createMapObjectConfig'
import { IMapObject, ITiledMapObject, MapObject } from '../components/MapObject'

export default function*(level: ILevelComponents, layer: string, depth: number, offsetX?: number, offsetY?: number): Iterable<IMapObject | OverlayParticles> {

    const tilemap = level.map
    const scene = level.scene
    if (!tilemap.getObjectLayer(layer)) { return }
    for (let obj of tilemap.getObjectLayer(layer).objects) {
        let mobj: ITiledMapObject = { depth: depth, ...obj }
        mobj.x = mobj.x ? mobj.x + (offsetX || 0) : 0
        mobj.y = mobj.y ? mobj.y + (offsetY || 0) : 0

        switch (mobj.type) {
            // case "door":
            //     yield new Door( scene, mobj.x!, mobj.y!, mobj)
            //     break;
            // casela "overlap":
            //     yield new OverlapArea( scene, mobj.x!, mobj.y!, mobj);
            //     break;
            // case "item":
            //     yield new MapItem( scene, mobj.x!, mobj.y!, mobj);
            //     break;
            case "falling_leaves_emitter":
                yield new OverlayParticles({
                    scene: level.scene,
                    follow: level.player,
                    texture: "falling_leaves",
                    frameEnd: 11,
                    anim: scene.anims.get("falling_leaves_anim_0")
                })
                break;
            case "item":
                yield new MapItem(level,mobj.x!,mobj.y!,mobj);
                break;
            case "canyon-swirl":
                yield new CanyonSwirl(createMapObjectConfig(mobj, level));
                break;
            default:
                yield new MapObject(createMapObjectConfig(mobj, level));
                break;
        }
    }
}
