import {
    Perlin,
    Signal,
    signalFromConfig,
} from "mapscripts/src/WorldGenerator";
import { Ref } from "preact";
import { useContext, useEffect, useRef, useState } from "preact/hooks";
import { Button } from "./components/Button";
import { Collapser } from "./components/Collapser";
import { Input } from "./components/Input";
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
    const [val, setVal] = useState({
        freq: 0.02,
        seed: 108,
        depth: 10,
    });
    return (
        <div>
            {isPerlin && (
                <>
                    <NumberSelector
                        min={0.0001}
                        max={0.5}
                        step={0.001}
                        name="Freq"
                        val={val.freq}
                        cb={(v) => {
                            setVal({ ...val, freq: v });
                        }}
                    />
                    <NumberSelector
                        min={0}
                        max={Infinity}
                        step={1}
                        name="Seed"
                        val={val.seed}
                        cb={(v) => {
                            setVal({ ...val, seed: v });
                        }}
                    />
                    {/* <Selector
                        items={["hhl", "ty", "hgf", "sad"]}
                        cb={(_d) => undefined}
                        name="Test"
                    />
                     <SelectBox
                        items={["hhl", "ty", "hgf", "sad"]}
                        cb={(d) => setVal(d)}
                        name="Test"
                        val={val}
                    /> */}
                    <Input name="hh" />
                    <Button
                        label="Gen"
                        cb={() => {
                            setSig(
                                signalFromConfig({
                                    type: "perlin",
                                    params: [
                                        ["freq", val.freq],
                                        ["seed", val.seed],
                                        ["depth", val.depth],
                                    ],
                                })
                            );
                        }}
                    />
                </>
            )}
        </div>
    );
}

export function SignalViewCanvas({
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
    const gen = (mounted: boolean) => {
        toDataURL(
            async (w, h, ctx) => {
                await sig.renderToContext(w, h, ctx);
            },
            w,
            h
        ).then((imgs) => {
            if (mounted) {
                setImg(imgs);
                setLoading(false);
            }
        });
    };

    useEffect(() => {
        setLoading(true);
    }, [w, h, sig]);
    useEffect(() => {
        let mounted = true;
        if (loading) gen(mounted);
        return () => (mounted = false);
    }, [loading]);

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
