export function Button({ cb, label }: { cb: () => void; label: string }) {
    return (
        <button className="py-1 px-8 rounded bg-gray-200" onClick={cb}>
            {label}
        </button>
    );
}
