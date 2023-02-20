import { JSXInternal } from "preact/src/jsx";
import { ControlPanel } from "./ControlPanel";
import "./number-selector.css";

export function NumberSelector({
    min,
    max,
    step,
    cb,
    val,
    name,
    ty,
    isFloat,
}: {
    min: number;
    max: number;
    step: number;
    cb: (v: number) => void;
    val: number;
    name: string;
    ty?: string;
    isFloat?: boolean;
}) {
    const inputName = name.toLowerCase();
    return (
        <ControlPanel name={name}>
            <button
                className="bg-white border-r-0 w-8"
                onClick={(_) => {
                    cb(val - step);
                }}
            >
                ←
            </button>
            <input
                className="pl-4 w-32"
                type={ty || "number"}
                min={min.toString()}
                max={max.toString()}
                step={step}
                name={inputName}
                value={val}
                onKeyDown={(k) => {
                    k.preventDefault();
                    if (k.key === "ArrowDown") {
                        cb(val - step);
                    } else if (k.key === "ArrowUp") {
                        cb(val + step);
                    }
                }}
                onInput={(v) => {
                    if (v.target instanceof HTMLInputElement) {
                        cb(
                            isFloat
                                ? parseFloat(v.target.value)
                                : parseInt(v.target.value)
                        );
                    }
                }}
            />
            <button
                className="bg-white w-8 border-l-0 rounded-r"
                onClick={(_) => {
                    cb(val + step);
                }}
            >
                →
            </button>
        </ControlPanel>
    );
}
