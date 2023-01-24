export function SelectBox({
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
            <p
                className="px-4 py-0.5 grow bg-gray-200 border border-black border-b-0"
                for={name}
            >
                {name}
            </p>
            <select
                name={name}
                onSelect={(v) => {
                    if (v.target instanceof HTMLSelectElement) {
                        cb(v.target.value);
                    }
                }}
                className="w-full bg-white border border-black px-4 py-0.5"
                multiple
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
