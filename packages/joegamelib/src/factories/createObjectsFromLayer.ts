import { createMapObjectConfig } from '../utils/createMapObjectConfig'
import { IMapObject, ITiledMapObject, MapObject } from '../components/MapObject'
import { LevelScene } from '../LevelScene'
import Character from '../Character'
import { TiledJsonProperty } from '../types/TiledRawJson'

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
    return !ob.properties?.find(
      (prop: { name: string; type: string; value: unknown }) =>
        prop.name === 'tileobject'
    )?.value
  })) {
    let mobj: ITiledMapObject = { depth: depth, ...obj }
    mobj.x = mobj.x ? mobj.x + (offsetX || 0) : 0
    mobj.y = mobj.y ? mobj.y + (offsetY || 0) : 0

    switch (mobj.type) {
      case 'char': {
        const props = Object.fromEntries(
          mobj.properties?.map((p: TiledJsonProperty) => [p.name, p.value])
        )
        console.log(mobj, props)
        yield new Character({
          scene: scene,
          x: mobj.x,
          y: mobj.y,
          name: mobj.name,
          texture: props.texture,
          anims: {
            north: props.animNorth,
            south: props.animSouth,
            east: props.animEast,
            west: props.animWest
          },
          speed: props.speed,
          body: {
            offsetY: props.body_off_y,
            offsetX: props.body_off_x,
            width: props.body_width,
            height: props.body_height
          },
          depth: mobj.depth,
          dashDistance: 16
        })
        break
      }

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
