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
        <div className={"px-6 py-2"}>
            <label
                className="px-4 py-0.5 grow bg-gray-200 border border-black border-r-0"
                for={name}
            >
                {name}
            </label>
            <select
                name={name}
                onSelect={(v) => {
                    if (v.target instanceof HTMLSelectElement) {
                        cb(v.target.value);
                    }
                }}
                className="w-48 bg-white border border-black px-4 py-0.5"
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
        </div>
    );
}
