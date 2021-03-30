"use strict";

var _interopRequireWildcard = require("@babel/runtime/helpers/interopRequireWildcard");

var _interopRequireDefault = require("@babel/runtime/helpers/interopRequireDefault");

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.default = loadAssets;

var _getKeyNames = require("./getKeyNames");

var _wikiData = _interopRequireDefault(require("./wikiData"));

var url = _interopRequireWildcard(require("url"));

/*
 * For loading assets from a LOADED raw tiled json
 * Both the tiled json and wikidata need to be available first
 */
function loadAssets(game, mapjsonpath) {
  return new Promise(function (res, reject) {
    var mapjson = game.cache.json.get((0, _getKeyNames.getMapKeyNameRaw)(mapjsonpath));
    var wikidata = (0, _wikiData.default)(game);
    if (!mapjson || !wikidata) reject("wikidata and mapjson is not already loaded!");
    var scene = game.scene.getScenes(true)[0]; // scene.load.setBaseURL(ASSETPATH)

    loadTilesets(scene, mapjson, mapjsonpath);
    loadObjectAssets(scene, mapjson, wikidata);
    scene.load.tilemapTiledJSON((0, _getKeyNames.getMapKeyName)(mapjsonpath), mapjson);
    loadDialogueFile(scene, mapjsonpath);
    scene.load.once('complete', function () {
      res(game);
    });
    scene.load.once('loaderror', function (file) {
      if (file.key != (0, _getKeyNames.getDialogueKeyName)(mapjsonpath)) {
        reject(file);
      } else {
        console.log("loading default dialogue file");
        scene.load.json((0, _getKeyNames.getDialogueKeyName)(mapjsonpath), 'assets/dialogues/default_dialogue.json');
      }
    });
    scene.load.start(); // res(game)
  });
}

function loadTilesets(scene, mapjson, path) {
  mapjson.tilesets.forEach(function (t) {
    if (t.image) {
      var fixpath = url.resolve(path, t.image);
      scene.load.spritesheet({
        key: t.name,
        url: fixpath,
        frameConfig: {
          frameWidth: t.tilewidth,
          frameHeight: t.tileheight,
          margin: t.margin,
          spacing: t.spacing
        }
      });
    } else if (t.tiles) {
      //it is an "image collection" tileset, store it by firstgid and all that
      t.tiles.forEach(function (tiles) {
        if (tiles.image) {
          var _fixpath = url.resolve(path, tiles.image);

          scene.load.image((t.firstgid + tiles.id).toString(), _fixpath);
        }
      });
    }
  });
}

function loadDialogueFile(scene, mapjsonpath) {
  scene.load.json((0, _getKeyNames.getDialogueKeyName)(mapjsonpath), "assets/dialogues/" + (0, _getKeyNames.getDialogueKeyName)(mapjsonpath) + '.json');
}

function loadObjectAssets(scene, mapjson, wikidata) {
  var characters = ['player', 'playerturtle', 'Moby'];
  var charGroups = [];
  var mapobjects = [];
  var platforms = []; //TODO absolte path

  mapjson.layers.forEach(function (l) {
    //getting the name of characters prestent on the map
    if (l.type === "objectgroup") {
      if (l.name === 'NPCs') {
        var _l$objects;

        (_l$objects = l.objects) === null || _l$objects === void 0 ? void 0 : _l$objects.forEach(function (n) {
          if (n.name) {
            characters.push(n.name);
          } else {
            var _n$properties$find$va, _n$properties, _n$properties$find;

            charGroups.push((_n$properties$find$va = (_n$properties = n.properties) === null || _n$properties === void 0 ? void 0 : (_n$properties$find = _n$properties.find(function (prop) {
              return prop.name === 'charGroup';
            })) === null || _n$properties$find === void 0 ? void 0 : _n$properties$find.value) !== null && _n$properties$find$va !== void 0 ? _n$properties$find$va : 'all');
            console.log(charGroups);
          }
        });
      } else if (l.name === 'Platforms') {
        var _l$objects2;

        (_l$objects2 = l.objects) === null || _l$objects2 === void 0 ? void 0 : _l$objects2.forEach(function (n) {
          platforms.push(n.type);
        });
      } else if (l.name === 'TweetConvos') {
        var _l$objects3;

        (_l$objects3 = l.objects) === null || _l$objects3 === void 0 ? void 0 : _l$objects3.forEach(function (n) {
          var _n$properties$find$va2, _n$properties2, _n$properties2$find;

          charGroups.push((_n$properties$find$va2 = (_n$properties2 = n.properties) === null || _n$properties2 === void 0 ? void 0 : (_n$properties2$find = _n$properties2.find(function (prop) {
            return prop.name === 'charGroup';
          })) === null || _n$properties2$find === void 0 ? void 0 : _n$properties2$find.value) !== null && _n$properties$find$va2 !== void 0 ? _n$properties$find$va2 : 'all');
        });
      } else {
        var _l$objects4;

        (_l$objects4 = l.objects) === null || _l$objects4 === void 0 ? void 0 : _l$objects4.forEach(function (n) {
          mapobjects.push(n.type);
        }); //we are in object group, but not NPCs, so there might be mapobjects here
      }
    }
  });
  characters = Array.from(new Set(characters));
  charGroups = Array.from(new Set(charGroups));
  mapobjects = Array.from(new Set(mapobjects));
  platforms = Array.from(new Set(platforms));
  var spritesheets = [];
  var images = [];
  characters.forEach(function (n) {
    var found = wikidata.character.get(n);

    if (found != undefined) {
      spritesheets.push(found.texture);
    }
  });
  charGroups.forEach(function (n) {
    var group = Array.from(wikidata.character).filter(function (item) {
      var _item$1$charGroups;

      return ((_item$1$charGroups = item[1].charGroups) === null || _item$1$charGroups === void 0 ? void 0 : _item$1$charGroups.includes(n)) || n === 'all';
    });

    if (group != undefined) {
      group.forEach(function (e) {
        return spritesheets.push(e[1].texture);
      }); // spritesheets.push(found.texture)
    }
  });
  mapobjects.forEach(function (mo) {
    var found = wikidata.mapobject.get(mo);

    if (found != undefined) {
      //TODO need req_otherthings too
      wikidata.mapobject[mo].req_spritesheet.forEach(function (sheet) {
        return spritesheets.push(sheet);
      });
      wikidata.mapobject[mo].req_image.forEach(function (image) {
        return images.push(image);
      });
    }
  });
  platforms.forEach(function (p) {
    var found = wikidata.platform.get(p);

    if (found != undefined) {
      spritesheets.push(found.texture);
    }
  });
  spritesheets = Array.from(new Set(spritesheets));
  images = Array.from(new Set(images));
  spritesheets.forEach(function (s) {
    var found = wikidata.spritesheet.get(s);

    if (found != undefined) {
      // found.url = url.resolve(TILEMAPDIR, found.url)
      scene.load.spritesheet(found);
    }
  });
  images.forEach(function (i) {
    var found = wikidata.image.get(i);

    if (found != undefined) {
      // found.url = url.resolve(TILEMAPDIR, found.url)
      scene.load.image(found); // scene.textures.addBase64(found.key, await import(found.url))
    }
  });
}