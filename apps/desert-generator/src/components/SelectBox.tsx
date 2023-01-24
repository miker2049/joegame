import { ControlPanel } from "./ControlPanel";

export function SelectBox({
    items,
    cb,
    name,
    val,
}: {
    items: string[];
    cb: (s: string) => void;
    name: string;
    val: string;
}) {
    return (
        <ControlPanel name={name} oneLine={false}>
            <select
                name={name}
                onInput={(v) => {
                    if (v.target instanceof HTMLSelectElement) {
                        console.log(v.target.value);
                        cb(v.target.value);
                    }
                }}
                className="w-full bg-white border border-black px-4 py-0.5 rounded-b"
                multiple
                value={val}
            >
                {items.map((item) => {
                    const key = item.toLowerCase().replaceAll(" ", "_");
                    return (
                        <option key={key + "_component"} value={key}>
                            {item}
                        </option>
                    );
                })}
            </select>
        </ControlPanel>
    );
}
