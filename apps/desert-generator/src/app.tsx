import { useState, useRef, useEffect } from "preact/hooks";
import "./app.css";
import { JSXInternal } from "preact/src/jsx";
import { LevelView } from "./LevelView";
import {
    CircleFilter,
    Perlin,
    SnapFilter,
    WorldGenerator,
} from "./WorldGenerator";

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
    const canvasRef = useRef<HTMLCanvasElement>();
    const generate = async () => {
        if (canvasRef.current) {
            const ctx = canvasRef.current.getContext("2d");
            const pp = new WorldGenerator([new Perlin(0.01, 20)]);
            pp.signals[0].filters = [
                new CircleFilter(750, 250, 300, 0.7, 0),
                new CircleFilter(250, 750, 300, -0.8, 1),
                new SnapFilter(12, 2),
            ];
            if (ctx) pp.signals[0].renderToContext(width, height, ctx);
        }
    };

    const inputCb =
        (key: keyof Omit<PerlinState, "bubbles">, isFloat = false) =>
        (v: JSXInternal.TargetedEvent<HTMLInputElement>) => {
            const val = (v.target as HTMLInputElement).value;
            const out: PerlinState = { ...state };
            out[key] = isFloat ? parseFloat(val) : parseInt(val);
            setState(out);
        };
    return (
        <div className={""}>
            <canvas ref={canvasRef} width={width} height={height}></canvas>
            <NumberSelector
                min={0.00001}
                max={0.1}
                step={0.001}
                name="Freq"
                val={state.freq}
                cb={inputCb("freq", true)}
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
            {colors.length > 0 && <ColorList colors={colors} />}
            <div>
                <LevelView />
            </div>
        </div>
    );
}

function NumberSelector({
    min,
    max,
    step,
    cb,
    val,
    name,
    ty,
}: {
    min: number;
    max: number;
    step: number;
    cb: (v: JSXInternal.TargetedEvent<HTMLInputElement>) => void;
    val: number;
    name: string;
    ty?: string;
}) {
    const inputName = name.toLowerCase();
    return (
        <div className="">
            <label for={inputName}>{name}</label>
            <input
                type={ty || "number"}
                min={min.toString()}
                max={max.toString()}
                step={step}
                name={inputName}
                value={val}
                onInput={cb}
            />
            <input type="text" value={val}></input>
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
