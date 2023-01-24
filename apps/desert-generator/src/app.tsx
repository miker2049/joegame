import { useState, useRef, useEffect } from "preact/hooks";
import "./app.css";
import { JSXInternal } from "preact/src/jsx";
import { LevelView } from "./LevelView";
import {
    BinaryFilter,
    CircleFilter,
    Perlin,
    SignalMaskFilter,
    SnapFilter,
    WorldGenerator,
} from "mapscripts/src/WorldGenerator";
import TiledRawJSON from "joegamelib/src/types/TiledRawJson";
import { SignalView } from "./SignalView";
import { NumberSelector } from "./components/NumberSelector";

interface PerlinState {
    freq: number;
    depth: number;
    snaps: number;
    bubbles: { radius: number; x: number; y: number; amount: number }[];
}

export function App() {
    const [state, setState] = useState<PerlinState>({
        freq: 0.003,
        depth: 5,
        snaps: 12,
        bubbles: [],
    });
    const [isLoading, setIsLoading] = useState(false);
    const [colors, setColors] = useState<number[]>([]);
    const width = 1000,
        height = 1000;
    const canvasRef = useRef<HTMLCanvasElement>(null);
    const generate = async () => {
        if (canvasRef.current) {
            const ctx = canvasRef.current.getContext("2d");
            const tm: TiledRawJSON = await (
                await fetch("/assets/maps/desert/desert-stamps2.json")
            ).json();
            const pp = new WorldGenerator(tm);
            pp.signals.push(
                new Perlin(state.freq, state.depth, 108, [
                    /* new SnapFilter(state.snaps, 2), */
                    new BinaryFilter(0.28, 2),
                ])
            );
            if (ctx) pp.signals[0].renderToContext(width, height, ctx);
        }
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
            {/* <canvas ref={canvasRef} width={width} height={height}></canvas>
            <NumberSelector
                min={0.00001}
                max={0.1}
                step={0.001}
                name="Freq"
                val={state.freq}
                cb={inputCb("freq", true)}
                isFloat
            />
            <NumberSelector
                min={1}
                max={32}
                step={1}
                name="Snaps"
                val={state.snaps}
                cb={inputCb("snaps")}
            />
            <NumberSelector
                min={1}
                max={30}
                step={1}
                name="Depth"
                val={state.depth}
                cb={inputCb("depth")}
            />
            <button
                name="generate"
                onClick={() => {
                    setIsLoading(true);
                    generate().then(() => {
                        console.log("finished generating");
                        setIsLoading(false);
                    });
                }}
                disabled={isLoading}
            >
                generate
            </button>
            {isLoading && <p> IS GENERATING </p>}
            {colors.length > 0 && <ColorList colors={colors} />} */}
            <div>
                <SignalView
                    w={500}
                    h={500}
                    sig={new Perlin(0.01, 20, 123132)}
                    setSig={(v) => undefined}
                />
            </div>
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
