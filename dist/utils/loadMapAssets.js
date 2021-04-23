import { getMapKeyNameRaw, getMapKeyName, getDialogueKeyName } from "./getKeyNames";
import wikiData from './wikiData';
import * as url from 'url';
/*
 * For loading assets from a LOADED raw tiled json
 * Both the tiled json and wikidata need to be available first
 */
export default function loadAssets(game, mapjsonpath) {
    return new Promise((res, reject) => {
        const mapjson = game.cache.json.get(getMapKeyNameRaw(mapjsonpath));
        const wikidata = wikiData(game);
        if (!mapjson || !wikidata)
            reject("wikidata and mapjson is not already loaded!");
        const scene = game.scene.getScenes(true)[0];
        // scene.load.setBaseURL(ASSETPATH)
        loadTilesets(scene, mapjson, mapjsonpath);
        loadObjectAssets(scene, mapjson, wikidata);
        scene.load.tilemapTiledJSON(getMapKeyName(mapjsonpath), mapjson);
        loadDialogueFile(scene, mapjsonpath);
        scene.load.once('complete', () => { res(game); });
        scene.load.once('loaderror', (file) => {
            if (file.key != getDialogueKeyName(mapjsonpath)) {
                reject(file);
            }
            else {
                // console.log("loading default dialogue file")
                // scene.load.json(getDialogueKeyName(mapjsonpath), 'assets/dialogues/default_dialogue.json')
            }
        });
        scene.load.start();
        // res(game)
    });
}
function loadTilesets(scene, mapjson, path) {
    mapjson.tilesets.forEach((t) => {
        if (t.image) {
            const fixpath = url.resolve(path, t.image);
            scene.load.spritesheet({ key: t.name, url: fixpath, frameConfig: { frameWidth: t.tilewidth, frameHeight: t.tileheight, margin: t.margin, spacing: t.spacing } });
        }
        else if (t.tiles) {
            //it is an "image collection" tileset, store it by firstgid and all that
            t.tiles.forEach((tiles) => {
                if (tiles.image) {
                    const fixpath = url.resolve(path, tiles.image);
                    scene.load.image((t.firstgid + tiles.id).toString(), fixpath);
                }
            });
        }
    });
}
function loadDialogueFile(scene, mapjsonpath) {
    scene.load.json(getDialogueKeyName(mapjsonpath), "assets/dialogues/" + getDialogueKeyName(mapjsonpath) + '.json');
}
function loadObjectAssets(scene, mapjson, wikidata) {
    let characters = [];
    let charGroups = [];
    let mapobjects = [];
    let platforms = [];
    //TODO absolte path
    mapjson.layers.forEach((l) => {
        var _a, _b, _c, _d, _e;
        //getting the name of characters prestent on the map
        if (l.type === "objectgroup") {
            if (l.name === 'NPCs') {
                (_a = l.objects) === null || _a === void 0 ? void 0 : _a.forEach((n) => {
                    var _a, _b, _c;
                    if (n.name) {
                        characters.push(n.name);
                    }
                    else {
                        charGroups.push((_c = (_b = (_a = n.properties) === null || _a === void 0 ? void 0 : _a.find(prop => prop.name === 'charGroup')) === null || _b === void 0 ? void 0 : _b.value) !== null && _c !== void 0 ? _c : 'all');
                        console.log(charGroups);
                    }
                });
            }
            else if (l.name === 'Platforms') {
                (_b = l.objects) === null || _b === void 0 ? void 0 : _b.forEach((n) => {
                    platforms.push(n.type);
                });
            }
            else if (l.name === 'TweetConvos') {
                (_c = l.objects) === null || _c === void 0 ? void 0 : _c.forEach((n) => {
                    var _a, _b, _c;
                    charGroups.push((_c = (_b = (_a = n.properties) === null || _a === void 0 ? void 0 : _a.find(prop => prop.name === 'charGroup')) === null || _b === void 0 ? void 0 : _b.value) !== null && _c !== void 0 ? _c : 'all');
                });
            }
            else if (l.name === 'Player') {
                (_d = l.objects) === null || _d === void 0 ? void 0 : _d.forEach((n) => {
                    var _a, _b, _c;
                    charGroups.push((_c = (_b = (_a = n.properties) === null || _a === void 0 ? void 0 : _a.find(prop => prop.name === 'charGroup')) === null || _b === void 0 ? void 0 : _b.value) !== null && _c !== void 0 ? _c : 'all');
                });
            }
            else {
                (_e = l.objects) === null || _e === void 0 ? void 0 : _e.forEach((n) => {
                    mapobjects.push(n.type);
                });
                //we are in object group, but not NPCs, so there might be mapobjects here
            }
        }
    });
    characters = Array.from(new Set(characters));
    charGroups = Array.from(new Set(charGroups));
    mapobjects = Array.from(new Set(mapobjects));
    platforms = Array.from(new Set(platforms));
    let spritesheets = [];
    let images = [];
    characters.forEach((n) => {
        const found = wikidata.character.get(n);
        if (found != undefined) {
            spritesheets.push(found.texture);
        }
    });
    charGroups.forEach((n) => {
        const group = Array.from(wikidata.character).filter(item => { var _a; return ((_a = item[1].charGroups) === null || _a === void 0 ? void 0 : _a.includes(n)) || n === 'all'; });
        if (group != undefined) {
            group.forEach(e => spritesheets.push(e[1].texture));
            // spritesheets.push(found.texture)
        }
    });
    mapobjects.forEach((mo) => {
        const found = wikidata.mapobject.get(mo);
        if (found != undefined) {
            //TODO need req_otherthings too
            found.req_spritesheet.forEach((sheet) => spritesheets.push(sheet));
            found.req_image.forEach((image) => images.push(image));
        }
    });
    platforms.forEach((p) => {
        const found = wikidata.platform.get(p);
        if (found != undefined) {
            spritesheets.push(found.texture);
        }
    });
    spritesheets = Array.from(new Set(spritesheets));
    images = Array.from(new Set(images));
    spritesheets.forEach((s) => {
        const found = wikidata.spritesheet.get(s);
        if (found != undefined) {
            // found.url = url.resolve(TILEMAPDIR, found.url)
            scene.load.spritesheet(found);
        }
    });
    images.forEach((i) => {
        const found = wikidata.image.get(i);
        if (found != undefined) {
            // found.url = url.resolve(TILEMAPDIR, found.url)
            scene.load.image(found);
            // scene.textures.addBase64(found.key, await import(found.url))
        }
    });
}
//# sourceMappingURL=loadMapAssets.js.map