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
    console.log(coord, "coord");
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

export async function snapshotWang(
    input: DataGrid<number>,
    tm: TiledMap,
    tmpath: string,
    layer: string,
    x: number,
    y: number,
    width: number,
    height: number
): Promise<string> {
    const scaled = scaleGrid(input, 2);
    const mdata = makeWangMapFrom2DArr(scaled, tm, layer);
    return await snapshotMap(
        {
            mapPath: tmpath,
            mapData: mdata,
            noPlayer: true,
            gameConfigOverrides: {
                dom: {},
                parent: null as unknown as undefined,
                render: {
                    transparent: true,
                },
            },
        },
        {
            x,
            y,
            width,
            height,
        },
        { zoom: 1.5 }
    );
}

export async function getWangPreviews(
    wangmap: TiledRawJSON,
    wangmapPath: string
): Promise<[string, string][]> {
    const inp = DataGrid.fromGrid([
        [0, 0, 0],
        [0, 1, 0],
        [0, 0, 0],
    ]);
    return Promise.all(
        wangmap.layers.map<Promise<[string, string]>>((layer) => {
            const cvsize = 3 * 4 * 16;
            const margin = 24;
            console.log(layer.name);
            return snapshotWang(
                inp,
                new TiledMap(wangmap),
                wangmapPath,
                layer.name,
                margin,
                margin,
                cvsize - 2 * margin,
                cvsize - 2 * margin
            ).then((dataurl) => [layer.name, dataurl]);
        })
    );
}
