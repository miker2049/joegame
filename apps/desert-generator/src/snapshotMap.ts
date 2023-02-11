import { LevelScene } from "joegamelib/src/LevelScene";
import { loadLevel } from "joegamelib/src";
import TiledRawJSON from "joegamelib/src/types/TiledRawJson";
import loadMapJSON, { embedTilesets } from "joegamelib/src/utils/loadMapJSON";
import { objectsAndPack } from "mapscripts/src/saturator";
import { TiledMap } from "mapscripts/src/TiledMap";
import {
    DataGrid,
    makeWangMapFrom2DArr,
    scaleGrid,
} from "mapscripts/src/utils";

export async function snapshotMap(
    map: TiledRawJSON,
    config: {
        coord?: { x: number; y: number; width: number; height: number };
        camera?: Partial<{ x: number; y: number; zoom: number }>;
        gameConfig?: Phaser.Types.Core.GameConfig;
    }
): Promise<string> {
    const sceneKey = "wangscene" + `${Math.random()}`.slice(3);
    const { coord, camera, gameConfig } = config;
    const scene = await loadLevel(map, sceneKey);
    const img = await new Promise<string>((res, rej) => {
        if (camera) {
            scene.cameras.main.centerOn(camera.x || 0, camera.y || 0);
            scene.cameras.main.setZoom(camera.zoom || 1);
        }
        scene.renderer.snapshotArea(
            coord?.x || 0,
            coord?.y || 0,
            coord?.width || scene.game.renderer.width,
            coord?.height || scene.game.renderer.height,
            (im) => {
                scene.game.destroy(true, false);
                res((im as HTMLImageElement).src);
            }
        );
    });
    console.log("img done", img);

    return img;
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
    return await snapshotMap(mdata, {
        coord: {
            x,
            y,
            width,
            height,
        },
        camera: { zoom: 1.5 },
    });
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
