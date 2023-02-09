import { CanyonSwirl } from '../components/CanyonSwirl'
// import MapItem from 'components/MapItem'
import OverlapArea from '../components/OverlapArea'
import OverlayParticles from '../components/OverlayParticles'
import { ILevelComponents } from '../ILevel'
import { createMapObjectConfig } from '../utils/createMapObjectConfig'
import { IMapObject, ITiledMapObject, MapObject } from '../components/MapObject'
import { LevelScene } from '../LevelScene'

export default function* (
  scene: LevelScene,
  layer: string,
  depth: number,
  offsetX?: number,
  offsetY?: number
): Iterable<IMapObject> {
  const tilemap = scene.map
  if (!tilemap || !tilemap.getObjectLayer(layer)) {
    console.log('Problem iterating objects')
    return
  }
  for (let obj of tilemap.getObjectLayer(layer).objects.filter((ob) => {
    //For now, filter out "TileObjects", but we need to make there bodies
    // eventually
    return !ob.properties.find((prop) => prop.name === 'tileobject')?.value
  })) {
    let mobj: ITiledMapObject = { depth: depth, ...obj }
    mobj.x = mobj.x ? mobj.x + (offsetX || 0) : 0
    mobj.y = mobj.y ? mobj.y + (offsetY || 0) : 0

    switch (mobj.type) {
      // case "door":
      //     yield new Door( scene, mobj.x!, mobj.y!, mobj)
      //     break;
      // case 'overlap':
      //   yield new OverlapArea({
      //     x: mobj.x,
      //     y: mobj.y,
      //     width: mobj.width ?? 5,
      //     height: mobj.height ?? 5,
      //     emit: 'hmm',
      //     level
      //   })
      //   break
      // case "item":
      //     yield new MapItem( scene, mobj.x!, mobj.y!, mobj);
      //     break;
      // case "falling_leaves_emitter":
      //     yield new OverlayParticles({
      //         scene: level.scene,
      //         follow: level.player,
      //         x: 3,
      //         y: 3,
      //         texture: "falling_leaves",
      //         frameEnd: 11,
      //         anim: scene.anims.get("falling_leaves_anim_0")
      //     })
      //     break;
      // case "item":
      //     yield new MapItem(level,mobj.x!,mobj.y!,mobj);
      //     break;
      // case 'canyon-swirl':
      //   yield new CanyonSwirl(createMapObjectConfig(mobj, level))
      //   break
      default:
        yield new MapObject(createMapObjectConfig(mobj, scene))
        break
    }
  }
}
