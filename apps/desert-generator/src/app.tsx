import { useState, useRef, useEffect } from "preact/hooks";
import * as Comlink from "comlink";
import "./app.css";
import { BinaryFilter, Perlin } from "mapscripts/src/WorldGenerator";
import TiledRawJSON from "joegamelib/src/types/TiledRawJson";
import { SignalView } from "./SignalView";
import { Collapser } from "./components/Collapser";
import { WangSetView } from "./WangSetView";
import { WangLayerView } from "./WangLayerView";
import { TiledMap } from "mapscripts/src/TiledMap";

interface PerlinState {
    freq: number;
    depth: number;
    snaps: number;
    bubbles: { radius: number; x: number; y: number; amount: number }[];
}
const PATH = "/assets/maps/desert/desert-stamps2.json";
export function App() {
    const [state, setState] = useState<PerlinState>({
        freq: 0.003,
        depth: 5,
        snaps: 12,
        bubbles: [],
    });
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

    const inputCb =
        (key: keyof Omit<PerlinState, "bubbles">, isFloat = false) =>
        (v: number) => {
            const out: PerlinState = { ...state };
            out[key] = v;
            setState(out);
        };

    return (
        <div className={""}>
            {tm && (
                <Collapser name={"Signals"}>
                    <SignalView
                        sig={new Perlin(0.01, 5, 123123)}
                        setSig={(s) => undefined}
                        w={500}
                        h={500}
                    />
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
                                new BinaryFilter(0.5, 2322),
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
