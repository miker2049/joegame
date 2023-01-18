import { ILevelComponents } from "joegamelib/src/ILevel";
import { LevelConfig } from "joegamelib/src/LevelConfig";
import { loadMap } from "joegamelib/src/loadMap";
import TiledRawJSON from "joegamelib/src/types/TiledRawJson";
import loadMapJSON from "joegamelib/src/utils/loadMapJSON";
import { TiledMap } from "mapscripts/src/TiledMap";
import {
    DataGrid,
    makeWangMapFrom2DArr,
    scaleGrid,
} from "mapscripts/src/utils";

export async function snapshotMap(
    config: LevelConfig,
    coord?: { x: number; y: number; width: number; height: number },
    camera?: Partial<{ x: number; y: number; zoom: number }>
): Promise<string> {
    return new Promise((res, rej) =>
        loadMap(config).then(([l, facade]) => {
            if (camera) {
                l.scene.cameras.main.centerOn(camera.x || 0, camera.y || 0);
                l.scene.cameras.main.setZoom(camera.zoom || 1);
            }
            l.scene.game.renderer.snapshotArea(
                coord?.x || 0,
                coord?.y || 0,
                coord?.width || l.scene.game.renderer.width,
                coord?.height || l.scene.game.renderer.height,
                (im) => {
                    facade.shutdown(l);
                    res((im as HTMLImageElement).src);
                }
            );
        })
    );
}

export async function getWangPreviews(wangmapPath: string): Promise<string[]> {
    const wangmap: Awaited<TiledRawJSON> = await (
        await fetch(wangmapPath)
    ).json();
    const inp = scaleGrid(
        DataGrid.fromGrid([
            [0, 0, 0],
            [0, 1, 0],
            [0, 0, 0],
        ]),
        2
    );
    return Promise.all(
        wangmap.layers.map<Promise<string>>((layer) => {
            console.log(layer.name);
            const mdata = makeWangMapFrom2DArr(
                inp!,
                new TiledMap(wangmap),
                layer.name
            );
            const cvsize = 3 * 4 * 16;
            const margin = 24;
            return snapshotMap(
                {
                    mapPath: wangmapPath,
                    mapData: mdata,
                    noPlayer: true,
                    gameConfigOverrides: {
                        dom: {},
                        parent: null,
                        render: {
                            transparent: true,
                        },
                    },
                },
                {
                    x: margin,
                    y: margin,
                    width: cvsize - 2 * margin,
                    height: cvsize - 2 * margin,
                },
                { zoom: 1.5 }
            );
        })
    );
}
