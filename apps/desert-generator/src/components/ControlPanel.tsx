import { VNode } from "preact";
import "./controlpanel.css";
export function ControlPanel({
    name,
    children,
    oneLine = true,
}: {
    children: VNode[] | VNode;
    name: string;
    oneLine?: boolean;
}) {
    return (
        <div
            className={
                "px-4 py-2 max-w-xs panel hover:drop-shadow-md" +
                " " +
                (oneLine ? "flex" : "")
            }
        >
            <p
                className={
                    "grow bg-gray-200 border border-black px-2" +
                    " " +
                    (oneLine ? "rounded-l border-r-0" : "rounded-t border-b-0")
                }
            >
                {name}
            </p>
            {children}
        </div>
    );
}
