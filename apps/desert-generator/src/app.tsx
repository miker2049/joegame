import { useState, useRef, useEffect } from "preact/hooks";
import { perlin2d } from "mapscripts/src/perlin";
import { applyDistortBubble } from "mapscripts/src/utils";
import "./app.css";
import { JSXInternal } from "preact/src/jsx";
import { LevelView } from "./LevelView";

export function snapToDivision(n: number, div: number) {
    let out = 0;
    const segs = Array(div)
        .fill(0)
        .map((_, idx) => idx / div);
    segs.forEach((seg) => {
        if (seg < n) out = seg;
    });
    return out;
}

function perlin2canvas(
    ctx: CanvasRenderingContext2D,
    width: number,
    height: number,
    freq?: number,
    depth?: number,
    snaps?: number
) {
    for (let y = 0; y < height; y++) {
        for (let x = 0; x < width; x++) {
            const n = perlin2d(x, y, freq, depth);
            const b1 = applyDistortBubble(x, y, n, 800, 100, 500, 0.8);
            const b2 = applyDistortBubble(x, y, b1, 100, 800, 500, -0.8);
            const snapped = snapToDivision(b2, snaps || 12);
            const hex = ("0" + Math.floor(snapped * 255).toString(16)).slice(
                -2
            );
            const color = "#" + hex + hex + hex;
            ctx.fillStyle = color;
            ctx.fillRect(x, y, 1, 1);
        }
    }
    ctx.fillStyle = "#FF0000";
    ctx.fillRect(108 * 8, 108, 10, 10);
}

function getAllColorsFromCanvas(
    ctx: CanvasRenderingContext2D,
    width: number,
    height: number
) {
    const colors: number[] = [];
    const d = ctx.getImageData(0, 0, width, height);
    for (let i = 0; i < d.data.length; i += 4) {
        // Modify pixel data
        const r = d.data[i + 0];

        if (!colors.includes(r)) {
            colors.push(r);
        }
    }
    return colors;
}

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
            if (ctx) {
                perlin2canvas(
                    ctx,
                    width,
                    height,
                    state.freq,
                    state.depth,
                    state.snaps
                );
                setColors(getAllColorsFromCanvas(ctx, width, height));
            }
        }
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
                cb={(v) => {
                    if (v.target?.value) {
                        setState({
                            ...state,
                            freq: parseFloat(v.target.value),
                        });
                    }
                }}
            />
            <NumberSelector
                min={1}
                max={32}
                step={1}
                name="Snaps"
                val={state.snaps}
                cb={(v) => {
                    if (v.target?.value) {
                        setState({
                            ...state,
                            snaps: parseInt(v.target.value),
                        });
                    }
                }}
            />
            <NumberSelector
                min={1}
                max={30}
                step={1}
                name="Depth"
                val={state.depth}
                cb={(v) => {
                    if (v.target?.value) {
                        setState({
                            ...state,
                            depth: parseInt(v.target.value),
                        });
                    }
                }}
            />
            <button
                name="generate"
                onClick={() => {
                    setIsLoading(true);
                    generate().then(() => {
                        console.log("here");
                        setIsLoading(false);
                    });
                }}
                disabled={isLoading}
            >
                generate
            </button>
            {isLoading && <p> IS GENERATING </p>}
            {colors.length > 0 && <ColorList colors={colors} />}
            <LevelView />
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
