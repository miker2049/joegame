import { VNode } from "preact";
import "./controlpanel.css";
export function ControlPanel({
    name,
    children,
    oneLine = true,
    small = true,
}: {
    children: VNode[] | VNode;
    name: string;
    oneLine?: boolean;
    small?: boolean;
}) {
    return (
        <div
            className={
                "px-4 py-2  panel hover:drop-shadow-md" +
                " " +
                (oneLine ? "flex" : "") +
                " " +
                (small ? "max-w-xs" : "")
            }
        >
            <p
                className={
                    "grow bg-gray-200 border border-black px-4 grow" +
                    " " +
                    (oneLine
                        ? "rounded-l border-r-0"
                        : "rounded-t border-b-0") +
                    " " +
                    (small ? "text-base" : "text-lg")
                }
            >
                {name}
            </p>
            {children}
        </div>
    );
}
