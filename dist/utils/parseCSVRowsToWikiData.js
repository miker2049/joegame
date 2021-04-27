import { parse as csvParse } from 'papaparse';
import { createTmpData } from './parseWikiData';
export function parseCSVRowsToWikiData(raw) {
    let parsed = csvParse(raw, { dynamicTyping: true });
    let tmpdata = createTmpData();
    parsed.data.forEach((row) => {
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
                            spacing: row[8],
                        }
                    });
                    break;
                }
                case 'image': {
                    row = row;
                    tmpdata.image.set(row[2], {
                        key: row[2],
                        url: row[3],
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
                            west: row[7],
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
                        groundTiles: `${row[4]}`.split(';').map(i => Number.parseInt(i)),
                        edgeTiles: `${row[5]}`.split(';').map(i => Number.parseInt(i))
                    });
                    break;
                }
                case 'mapobject': {
                    row = row;
                    tmpdata.mapobject.set(row[2], {
                        name: row[2],
                        req_spritesheet: `${row[3]}`.split(';'),
                        req_image: `${row[4]}`.split(';')
                    });
                    break;
                }
                case 'convoManifest': {
                    row = row;
                    tmpdata.convoManifest = row[2];
                    break;
                }
            }
        }
    });
    return tmpdata;
}
//# sourceMappingURL=parseCSVRowsToWikiData.js.map