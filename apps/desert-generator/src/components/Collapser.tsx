import { VNode } from "preact";
import { useState } from "preact/hooks";
import "./collapser.css";
export function Collapser({
    name,
    children,
}: {
    children: VNode;
    name: string;
}) {
    const [collapsed, setCollapsed] = useState(true);
    return (
        <div className={"rounded border-black border"}>
            <div className="flex bg-gray-200 p-2">
                <p className={"py-2 text-lg grow"}>{name}</p>
                <button
                    onClick={() => setCollapsed(!collapsed)}
                    className={
                        "p-2 text-lg rounded border-0 bg-gray-200 hover:bg-gray-300 transition-all duration-500"
                    }
                >
                    <Chev up={collapsed} />
                </button>
            </div>
            <div
                className={`collapser ${
                    collapsed ? "" : "expanded"
                } overflow-clip`}
            >
                {children}
            </div>
        </div>
    );
}

// HeroIcon
function Chev({ up }: { up: boolean }) {
    return (
        <svg
            className={`w-6 h-6 rotater ${up ? "down" : ""}`}
            xmlns="http://www.w3.org/2000/svg"
            fill="none"
            viewBox="0 0 24 24"
            strokeWidth={1.5}
            stroke="currentColor"
        >
            <path
                strokeLinecap="round"
                strokeLinejoin="round"
                d="M19.5 8.25l-7.5 7.5-7.5-7.5"
            />
        </svg>
    );
}
