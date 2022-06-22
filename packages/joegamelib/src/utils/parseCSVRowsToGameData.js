"use strict";
exports.__esModule = true;
exports.parseCSVRowsToGameData = void 0;
var papaparse_1 = require("papaparse");
var parseWikiData_1 = require("./parseWikiData");
function parseCSVRowsToGameData(raw) {
    var parsed = (0, papaparse_1.parse)(raw, { dynamicTyping: true });
    var tmpdata = (0, parseWikiData_1.createTmpData)();
    parsed.data.forEach(function (row) {
        if (row[0] !== -1) {
            switch (row[1]) {
                case 'spritesheet': {
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
                case 'image': {
                    row = row;
                    tmpdata.image.set(row[2], {
                        key: row[2],
                        url: row[3]
                    });
                    break;
                }
                case 'character': {
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
                case 'platform': {
                    row = row;
                    tmpdata.platform.set(row[2], {
                        name: row[2],
                        texture: row[3],
                        groundTiles: "".concat(row[4]).split(';').map(function (i) { return Number.parseInt(i); }),
                        edgeTiles: "".concat(row[5]).split(';').map(function (i) { return Number.parseInt(i); })
                    });
                    break;
                }
                case 'mapobject': {
                    row = row;
                    tmpdata.mapobject.set(row[2], {
                        name: row[2],
                        req_spritesheet: "".concat(row[3]).split(';'),
                        req_image: "".concat(row[4]).split(';')
                    });
                    break;
                }
                case 'convoManifest': {
                    row = row;
                    tmpdata.convoManifest = row[2];
                    break;
                }
                case 'htmlImage': {
                    row = row;
                    tmpdata.convoManifest = row[2];
                    break;
                }
                case 'soundFile': {
                    row = row;
                    tmpdata.sound.set(row[2], {
                        key: row[2],
                        url: row[3],
                        splitLength: row[4]
                    });
                    break;
                }
                case 'animatedTile': {
                    row = row;
                    tmpdata.animatedTiles.set(row[2], {
                        tileset: row[2],
                        // for all animated rows in a given map, in format id,id,id;id,id,id;
                        ids: "".concat(row[3]).split(';').map(function (chunk) { return chunk.split(',').map(function (i) { return Number.parseInt(i); }); })
                    });
                    break;
                }
            }
        }
    });
    return tmpdata;
}
exports.parseCSVRowsToGameData = parseCSVRowsToGameData;
