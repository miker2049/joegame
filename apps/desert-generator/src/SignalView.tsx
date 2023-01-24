import { Perlin, Signal } from "mapscripts/src/WorldGenerator";
import { Ref } from "preact";
import { useContext, useEffect, useRef, useState } from "preact/hooks";
import { Collapser } from "./components/Collapser";
import { NumberSelector } from "./components/NumberSelector";
import { SelectBox } from "./components/SelectBox";
import { Selector } from "./components/Selector";
import { toDataURL } from "./utils";
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
        <Collapser name={"Signals"}>
            <div className="flex p-6">
                <div className="grow p-4 bg-blue-300">
                    <SignalViewCanvas w={w} h={h} sig={sig} />
                </div>
                <div className=" mx-8 bg-red-300">
                    <SignalControls sig={sig} setSig={setSig} />
                </div>
            </div>
        </Collapser>
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
    const [val, setVal] = useState("hhl");
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
                            console.log(v);
                        }}
                    />
                    <NumberSelector
                        min={0}
                        max={Infinity}
                        step={1}
                        name="Seed"
                        val={0}
                        cb={(v) => {
                            console.log(v);
                        }}
                    />
                    <Selector
                        items={["hhl", "ty", "hgf", "sad"]}
                        cb={(_d) => undefined}
                        name="Test"
                    />
                    <SelectBox
                        items={["hhl", "ty", "hgf", "sad"]}
                        cb={(d) => setVal(d)}
                        name="Test"
                        val={val}
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
    const [img, setImg] = useState<string>();
    const render = async () => {
        if (!img) {
            const imgs = await toDataURL(
                async (w, h, ctx) => {
                    await sig.renderToContext(w, h, ctx);
                },
                w,
                h
            );
            setImg(imgs);
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
            <img src={img}></img>
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
