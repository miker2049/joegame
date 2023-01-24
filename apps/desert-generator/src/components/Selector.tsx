import { ControlPanel } from "./ControlPanel";

export function Selector({
    items,
    cb,
    name,
}: {
    items: string[];
    cb: (s: string) => void;
    name: string;
}) {
    return (
        <ControlPanel name={name}>
            <select
                name={name}
                onSelect={(v) => {
                    if (v.target instanceof HTMLSelectElement) {
                        cb(v.target.value);
                    }
                }}
                className="w-48 bg-white border border-black px-4 py-1 rounded-r"
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
