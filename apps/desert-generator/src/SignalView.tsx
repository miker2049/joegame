import { Signal } from "mapscripts/src/WorldGenerator";
import { Ref } from "preact";
import { useContext, useEffect, useRef, useState } from "preact/hooks";
export function SignalView({
    sig,
    w,
    h,
}: {
    sig: Signal;
    w: number;
    h: number;
}) {
    return (
        <div className="flex p-6">
            <div className="grow p-4 bg-blue-300">
                <SignalViewCanvas w={500} h={500} sig={sig} />
            </div>
            <div className=" mx-8 bg-red-300"></div>
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
    sig: Signal;
}) {
    const [loading, setLoading] = useState(true);
    const cnv = useRef<HTMLCanvasElement>();
    const tick = useRef<ReturnType<typeof setInterval>>();

    const render = () => {
        if (cnv.current) {
            const ctx = cnv.current.getContext("2d");
            if (ctx)
                return sig.renderToContext(w, h, ctx).then((_) => {
                    console.log("done render");
                });
        }
    };

    useEffect(() => {
        const p = render();

        if (p) p.then(() => setLoading(false));
    });

    return (
        <div>
            {loading && (
                <div
                    style={{
                        width: w,
                        height: h,
                        backgroundColor: "#0009",
                        padding: 10,
                        position: "relative",
                        top: 0,
                    }}
                >
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
