// import 'phaser'
// import TiledRawJson from '../types/TiledRawJson'
// import { getMapKeyNameRaw } from '../utils/getKeyNames'
import { MapObject } from '../components/MapObject';
// import Door from '../components/Door'
// import OverlapArea from '../components/OverlapArea'
// import MapItem from '../components/MapItem'
// // import ShinyRock from '../components/ShinyRock'
// import getDepthMap from '../utils/getDepthMap'
export default function* (tilemap, layer, depth, offsetX, offsetY) {
    if (!tilemap.getObjectLayer(layer)) {
        return;
    }
    const scene = tilemap.scene;
    for (let obj of tilemap.getObjectLayer(layer).objects) {
        let mobj = Object.assign({ depth: depth }, obj);
        mobj.x = mobj.x ? mobj.x + (offsetX || 0) : 0;
        mobj.y = mobj.y ? mobj.y + (offsetY || 0) : 0;
        switch (mobj.type) {
            // case "door":
            //     yield new Door( scene, mobj.x!, mobj.y!, mobj)
            //     break;
            // case "overlap":
            //     yield new OverlapArea( scene, mobj.x!, mobj.y!, mobj);
            //     break;
            // case "item":
            //     yield new MapItem( scene, mobj.x!, mobj.y!, mobj);
            //     break;
            // case "shinyrock":
            //     yield new ShinyRock(scene,mobj.x!,mobj.y!,mobj);
            //     break;
            default: yield new MapObject(scene, tilemap, mobj.x, mobj.y, mobj);
        }
    }
}
//# sourceMappingURL=createObjectsFromLayer.js.map