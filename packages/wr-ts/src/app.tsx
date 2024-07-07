import { useState } from "preact/hooks";
import "./app.css";
import data from "./n34w107.npy.1150.json";

const normalizeElevs = (d: number[]) => {
    const minVal = Math.min(...d);
    return d.map((it) => it - minVal);
};

const elevs = normalizeElevs(data);

const makeView = (x: number, y: number, w: number, h: number, d = data) => {
    const cells = Array(w).fill(0);
    const rows = Array(h).fill(0);
    return rows.map((_, rowi) =>
        cells.map((_, c) => {
            const row = h - rowi;
            if (row + y > d[c + x]) return "\u{2002}";
            else return "#";
        })
    );
};

function ViewTable({
    buff,
    keyCB,
}: {
    buff: string[][];
    keyCB: (ev: KeyboardEvent) => void;
}) {
    return (
        <table id="game-table" tabIndex={-1} onKeyDown={keyCB}>
            <tbody>
                {buff.map((row) => (
                    <tr>
                        {row.map((cell) => (
                            <td>{cell}</td>
                        ))}
                    </tr>
                ))}
            </tbody>
        </table>
    );
}

export function App() {
    // console.log(elevs);
    const [view, setView] = useState({ x: 0, y: 0, w: 80, h: 35 });

    const handleKeyEv = (ev: KeyboardEvent) => {
        switch (ev.key) {
            case "ArrowUp": {
                ev.preventDefault();
                setView((v) => ({ ...v, y: v.y + 1 }));
                break;
            }
            case "ArrowDown": {
                ev.preventDefault();
                setView((v) => ({ ...v, y: v.y - 1 }));
                break;
            }
            case "ArrowLeft": {
                ev.preventDefault();
                setView((v) => ({ ...v, x: v.x - 1 }));
                break;
            }
            case "ArrowRight": {
                ev.preventDefault();
                setView((v) => ({ ...v, x: v.x + 1 }));
            }
        }
    };
    // console.log(data);
    return (
        <div>
            <ViewTable
                buff={makeView(view.x, view.y, view.w, view.h, elevs)}
                keyCB={handleKeyEv}
            />
            <p id="text-window">
                ({view.x},{view.y})
            </p>
        </div>
    );
}
