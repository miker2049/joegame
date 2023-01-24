import { JSXInternal } from "preact/src/jsx";
import "./number-selector.css";

export function NumberSelector({
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
        <div className="number-input px-4 py-2">
            <label
                className="px-4 grow bg-gray-200 border border-black border-r-0 py-1"
                for={inputName}
            >
                {name}
            </label>
            <button className="border border-black border-r-0 bg-white px-2 w-8 py-0.5">
                ←
            </button>
            <input
                className="number-input border border-black px-4 w-32 py-0.5"
                type={ty || "number"}
                min={min.toString()}
                max={max.toString()}
                step={step}
                name={inputName}
                value={val}
                onInput={cb}
            />
            <button className="border border-black border-l-0 bg-white px-2 w-8 py-0.5">
                →
            </button>
        </div>
    );
}
