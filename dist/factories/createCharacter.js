import Character from '../Character';
import wikiData from '../utils/wikiData';
import defaults from '../defaults';
export default function (name, x, y, level) {
    const chardata = wikiData(level.scene.game).character.get(name);
    // console.log(chardata)
    if (chardata) {
        const config = {
            level: level,
            x: x,
            y: y,
            name: name,
            texture: chardata.texture,
            anims: chardata.anims,
            speed: chardata.speed ? chardata.speed : defaults.speed,
            scale: chardata.scale ? chardata.scale : defaults.scale,
            dashDistance: chardata.dashDistance ? chardata.dashDistance : defaults.dashDistance,
            body: chardata.body ? chardata.body : { offsetX: 0, offsetY: 0 }
        };
        return new Character(config);
    }
    else {
        throw new TypeError(`couldnt get character ${name}`);
    }
}
//# sourceMappingURL=createCharacter.js.map