import { useState, useRef, useEffect } from "preact/hooks";
import * as Comlink from "comlink";
import "./app.css";
import {
    BinaryFilter,
    EdgeFilter,
    Perlin,
    Signal,
    Voronoi,
    VoronoiManhattan,
    VoronoiSquared,
} from "mapscripts/src/WorldGenerator";
import TiledRawJSON from "joegamelib/src/types/TiledRawJson";
import { SignalView } from "./SignalView";
import { Collapser } from "./components/Collapser";
import { WangSetView } from "./WangSetView";
import { WangLayerView } from "./WangLayerView";
import { TiledMap } from "mapscripts/src/TiledMap";

const PATH = "/assets/maps/desert/desert-stamps2.json";
export function App() {
    const _sig = new VoronoiManhattan(100);
    _sig.filters = [new EdgeFilter(_sig)];
    const [sig, setSig] = useState<Signal>(_sig);

    const [tm, setTm] = useState<TiledRawJSON>();

    useEffect(() => {
        let mounted = true;
        if (!tm) {
            getTileMap().then((tmm) => {
                if (mounted) {
                    setTm(tmm);
                }
            });
        }
        return () => {
            mounted = false;
        };
    });

    const getTileMap = async () => {
        return await (await fetch(PATH)).json();
    };

    return (
        <div className={""}>
            {tm && (
                <Collapser name={"Signals"}>
                    <SignalView sig={sig} setSig={setSig} w={500} h={500} />
                </Collapser>
            )}
            {tm && (
                <Collapser name={"Wang Sets"}>
                    <WangSetView tiledjson={tm} mappath={PATH} />
                </Collapser>
            )}
            {tm && (
                <Collapser name={"Wang Layers"}>
                    <WangLayerView
                        x={50}
                        y={10}
                        width={30}
                        height={35}
                        sig={
                            new Perlin(0.1, 5, 23243243, [
                                new BinaryFilter(0.5),
                            ])
                        }
                        tm={new TiledMap(tm)}
                        layername="grass"
                    />
                </Collapser>
            )}
        </div>
    );
}

function ColorList({ colors }: { colors: number[] }) {
    return (
        <ul>
            {colors.map((c, idx) => (
                <li key={c + idx}>{c}</li>
            ))}
        </ul>
    );
}
