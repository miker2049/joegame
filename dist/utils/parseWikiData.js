"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.parseCSVRowsToWikiData = parseCSVRowsToWikiData;
exports.parsewikidata = parsewikidata;

var _papaparse = require("papaparse");

var wikientries;

(function (wikientries) {
  wikientries[wikientries["character"] = 0] = "character";
  wikientries[wikientries["spritesheet"] = 1] = "spritesheet";
  wikientries[wikientries["image"] = 2] = "image";
  wikientries[wikientries["platform"] = 3] = "platform";
  wikientries[wikientries["mapobject"] = 4] = "mapobject";
})(wikientries || (wikientries = {}));

var createTmpData = function createTmpData() {
  return {
    spritesheet: new Map(),
    character: new Map(),
    image: new Map(),
    platform: new Map(),
    mapobject: new Map()
  };
};

function parseCSVRowsToWikiData(raw) {
  var parsed = (0, _papaparse.parse)(raw, {
    dynamicTyping: true
  });
  var tmpdata = createTmpData();
  parsed.data.forEach(function (row) {
    if (row[0] !== -1) {
      switch (row[1]) {
        case 'spritesheet':
          {
            row = row;
            tmpdata.spritesheet.set(row[2], {
              key: row[2],
              url: row[3],
              animLength: row[4] != null ? row[4] : undefined,
              frameConfig: {
                frameWidth: row[5],
                frameHeight: row[6],
                margin: row[7],
                spacing: row[8]
              }
            });
            break;
          }

        case 'image':
          {
            row = row;
            tmpdata.image.set(row[2], {
              key: row[2],
              url: row[3]
            });
            break;
          }

        case 'character':
          {
            row = row;
            tmpdata.character.set(row[2], {
              name: row[2],
              texture: row[3],
              anims: {
                north: row[4],
                south: row[5],
                east: row[6],
                west: row[7]
              },
              speed: row[8],
              dashDistance: row[9],
              scale: row[10],
              body: {
                offsetX: row[11],
                offsetY: row[12],
                width: row[13],
                height: row[14]
              },
              charGroups: [row[15], row[16]]
            });
            break;
          }

        case 'platform':
          {
            row = row;
            tmpdata.platform.set(row[2], {
              name: row[2],
              texture: row[3],
              groundTiles: row[4].split(',').map(function (i) {
                return Number.parseInt(i);
              }),
              edgeTiles: row[5].split(',').map(function (i) {
                return Number.parseInt(i);
              })
            });
            break;
          }

        case 'mapobject':
          {
            row = row;
            tmpdata.mapobject.set(row[2], {
              name: row[2],
              req_spritesheet: [row[3], row[4], row[5]],
              req_image: [row[6], row[7], row[8]]
            });
            break;
          }
      }
    }
  });
  return tmpdata;
}

function parsewikidata(rawwikidata) {
  var tmpdata = createTmpData();
  rawwikidata.forEach(function (page) {
    page.forEach(function (item) {
      if (item.type) {
        switch (item.type) {
          case wikientries[wikientries.character]:
            tmpdata.character.set(item.name, item);
            break;

          case wikientries[wikientries.spritesheet]:
            tmpdata.spritesheet.set(item.key, item);
            break;

          case wikientries[wikientries.image]:
            tmpdata.image.set(item.key || item.name, item);
            break;

          case wikientries[wikientries.platform]:
            tmpdata.platform.set(item.name, item);
            break;

          case wikientries[wikientries.mapobject]:
            tmpdata.mapobject.set(item.name, item);
            break;
        }
      }
    });
  });
  return tmpdata;
}
//# sourceMappingURL=parseWikiData.js.map