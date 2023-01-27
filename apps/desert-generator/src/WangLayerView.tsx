import * as Comlink from "comlink";
import { TiledMap } from "mapscripts/src/TiledMap";
import { GenericSignal, WangLayer } from "mapscripts/src/WorldGenerator";
import { useEffect, useState } from "preact/hooks";
import { SignalView, SignalViewCanvas } from "./SignalView";
import { snapshotMap, snapshotWang } from "./snapshotMap";

export function WangLayerView({
    x,
    y,
    width,
    height,
    sig,
    tm,
    layername,
}: {
    x?: number;
    y?: number;
    /**
     * Width in wang units, i.e. 4 16px tile units.
     */
    width: number;
    /**
     * Height in wang units, i.e. 4 16px tile units.
     */
    height: number;
    sig: GenericSignal;
    tm: TiledMap;
    layername: string;
}) {
    const [pic, setPic] = useState<string>();
    useEffect(() => {
        let mounted = true;
        if (tm) {
            const xx = x || 0,
                yy = y || 0;
            const wl = new WangLayer(layername, tm, sig);
            const lg = wl.getTilesRect(xx, yy, width, height);
            const map = TiledMap.createEmpty(
                lg.height(),
                lg.width,
                tm.getConf()
            );
            map.applyLgs([lg], "wang");

            console.log("gambit", map.getConf().tilewidth);
            snapshotMap(
                {
                    mapPath: "/assets/maps/desert/desert-stamps2.json",
                    mapData: map.getConf(),
                    noPlayer: true,
                    gameConfigOverrides: {
                        parent: null as unknown as undefined,
                        render: {
                            transparent: true,
                        },
                        scale: {
                            mode: Phaser.Scale.MAX_ZOOM,
                            width: lg.width * map.getConf().tilewidth,
                            height: lg.height() * map.getConf().tileheight,
                        },
                    },
                },
                {
                    x: xx,
                    y: yy,
                    width: lg.width * map.getConf().tilewidth,
                    height: lg.height() * map.getConf().tileheight,
                },
                { zoom: 1.5 }
            )
                .then((p) => {
                    if (mounted) {
                        console.log("d");
                        setPic(p);
                    }
                })
                .catch((e) => {
                    console.log(e);
                });
        }
        return () => (mounted = false);
    }, [sig, tm, layername, width, height]);
    return (
        <div className="">
            <SignalViewCanvas sig={sig} w={width * 4} h={height * 4} />
            <img src={pic}></img>
        </div>
    );
}
