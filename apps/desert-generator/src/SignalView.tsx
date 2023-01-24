import { Perlin, Signal } from "mapscripts/src/WorldGenerator";
import { Ref } from "preact";
import { useContext, useEffect, useRef, useState } from "preact/hooks";
import { NumberSelector } from "./components/NumberSelector";
import { SelectBox } from "./components/SelectBox";
import { Selector } from "./components/Selector";
export function SignalView({
    sig,
    setSig,
    w,
    h,
}: {
    sig: Signal | Perlin;
    setSig: (sig: Signal | Perlin) => void;
    w: number;
    h: number;
}) {
    return (
        <div className="flex p-6">
            <div className="grow p-4 bg-blue-300">
                <SignalViewCanvas w={w} h={h} sig={sig} />
            </div>
            <div className=" mx-8 bg-red-300">
                <SignalControls sig={sig} setSig={setSig} />
            </div>
        </div>
    );
}

function SignalControls({
    sig,
    setSig,
}: {
    sig: Signal | Perlin;
    setSig: (sig: Signal | Perlin) => void;
}) {
    const isPerlin = "freq" in sig;

    return (
        <div>
            {isPerlin && (
                <>
                    <NumberSelector
                        min={0.0001}
                        max={0.5}
                        step={0.001}
                        name="Freq"
                        val={0.01}
                        cb={(v) => {
                            const t = v.target?.value;
                            console.log(t);
                        }}
                    />
                    <NumberSelector
                        min={0}
                        max={Infinity}
                        step={1}
                        name="Seed"
                        val={0}
                        cb={(v) => {
                            const t = v.target?.value;
                            console.log(t);
                        }}
                    />
                    <Selector
                        items={["hhl", "ty", "hgf", "sad"]}
                        cb={(_d) => undefined}
                        name="Test"
                    />
                    <SelectBox
                        items={["hhl", "ty", "hgf", "sad"]}
                        cb={(_d) => undefined}
                        name="Test"
                    />
                </>
            )}
        </div>
    );
}

function SignalViewCanvas({
    w,
    h,
    sig,
}: {
    w: number;
    h: number;
    sig: Signal | Perlin;
}) {
    const [loading, setLoading] = useState(true);
    const cnv = useRef<HTMLCanvasElement>();

    const render = () => {
        if (cnv.current) {
            const ctx = cnv.current.getContext("2d");
            if (ctx) {
                ctx.fillStyle = "#0f0000";
                ctx.rect(0, 0, w, h);
                return sig.renderToContext(w, h, ctx).then((_) => {
                    console.log("done render");
                });
            }
        }
    };

    useEffect(() => {
        const p = render();
        if (p) p.then(() => setLoading(false));
    });

    return (
        <div className="relative">
            {loading && (
                <div className="absolute left-1 top-1 scale-50">
                    <Spinner />
                </div>
            )}
            <canvas
                height={h}
                width={w}
                ref={cnv as Ref<HTMLCanvasElement>}
            ></canvas>
        </div>
    );
}

export function Spinner() {
    return (
        <svg
            className="animate-spin m-2"
            width="50"
            height="50"
            xmlns="http://www.w3.org/2000/svg"
        >
            <circle cx="25" cy="25" r="25" fill="black" />
            <path
                d="M2 25 A22 22, 0, 0, 1, 25 2 "
                stroke="white"
                stroke-width="5"
            />
        </svg>
    );
}
