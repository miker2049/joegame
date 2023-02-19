import { Meadow } from "./meadow";
import "./style.css";
import m from "midi";
import workleturl from "midi/dist/synth-worklet.js?url";
import sfurl from "../gravis.sf3?url";

const COLORS_N = 16;

function hexToRGB(n: number) {
    return {
        r: n >> 16,
        g: (n >> 8) & 255,
        b: n & 255,
    };
}

// 55 97 56

const colors = Array(COLORS_N)
    .fill(0)
    .map((_, idx) => {
        const l = interpolate(56, 0, idx, COLORS_N);
        return `hsl(55,97%,${Math.floor(l)}%)`;
    });

function interpolate(
    startValue: number,
    endValue: number,
    stepNumber: number,
    lastStepNumber: number
): number {
    return ((endValue - startValue) * stepNumber) / lastStepNumber + startValue;
}

/*
 * Table work
 */
const TICK = 100;
const ROW_N = 8;
const COL_N = 16;
const app = document.querySelector<HTMLDivElement>("#app")!;

const NOOP = () => undefined;
interface Grid<T> {
    items: T[];
    getItem: (x: number, y: number) => T;
    width: number;
    height: number;
}

class TableGrid implements Grid<HTMLTableCellElement> {
    el: HTMLTableElement;
    items: HTMLTableCellElement[];
    width: number;
    height: number;
    constructor(w: number, h: number) {
        this.width = w;
        this.height = h;
        this.el = document.createElement("table");
        const rows = Array(h)
            .fill(0)
            .map((_) => document.createElement("tr"));
        this.cellCb.bind(this);
        this.items = Array(h * w)
            .fill(0)
            .map((_, idx) => {
                const el = document.createElement("td");
                el.onclick = () => this.cellCb(idx);
                return el;
            });

        rows.forEach((row, idx) => {
            for (let i = 0; i < COL_N; i++) {
                row.appendChild(this.items[idx * COL_N + i]);
            }
            this.el.appendChild(row);
        });
    }

    getItem(x: number, y: number) {
        return this.items[y * this.width + x];
    }

    setColor(x: number, y: number, color: string) {
        this.getItem(x, y).style.backgroundColor = color;
    }

    cellCb(c: number) {}
}

interface MeadowRender<C> {
    mp: Meadow;
    setColor: (x: number, y: number, color: C) => void;
    activeColor: C;
    inActiveColor: C;
    posColor: C;
}

class MeadowTableHTML extends TableGrid implements MeadowRender<string> {
    mp: Meadow;
    activeColor: string;
    inActiveColor: string;
    posColor: string;
    constructor(w: number, h: number, evCb: (i: number) => void) {
        super(w, h);
        this.mp = new Meadow(w, h, evCb, NOOP);
        this.inActiveColor = "black";
        this.activeColor = "hsl(55,96%,40%)";
        this.posColor = "hsl(55,96%,56%)";
        this.animate.bind(this);
    }
    cellCb(i: number) {
        const row = Math.floor(i / this.width);
        this.mp.setCount(row, i % this.width);
        this.renderRow(row);
    }
    renderRow(y: number) {
        const count = this.mp.rows[y].count;
        const pos = this.mp.rows[y].pos;
        // console.log(`y: ${y} count: ${count} pos ${pos} height: ${this.height}`)
        for (let i = this.width - 1; i >= 0; --i) {
            if (i <= count) this.setColor(i, y, this.activeColor);
            else this.setColor(i, y, this.inActiveColor);
        }
        this.setColor(pos, y, this.posColor);
    }
    animate() {
        for (let y = 0; y < this.height; y++) {
            this.renderRow(y);
        }
        this.mp.tick();
        this.animate.bind(this);
        setTimeout(() => requestAnimationFrame(() => this.animate()), 50);
    }
}

(async () => {
    const ac = new AudioContext();
    const midi = await m.create(ac, workleturl);
    await midi.loadFont(sfurl);
    const notes = [60, 62, 64, 67, 69, 72, 74, 76];
    midi.node.connect(ac.destination);
    const noteon = (i: number) => {
        const p = Math.floor(Math.random() * 48);
        ac.resume();
        midi.noteon(notes[i], p, 1);
        setTimeout(() => midi.noteoff(notes[i], p, 0), 1000);
    };
    const tt = new MeadowTableHTML(16, 8, noteon);
    tt.mp.toggleLink(0, 1);
    tt.mp.toggleLink(0, 2);
    tt.mp.toggleLink(2, 3);
    tt.mp.toggleLink(2, 4);
    tt.mp.toggleLink(4, 5);
    tt.mp.toggleLink(0, 6);
    tt.mp.toggleLink(4, 7);

    document.querySelector("#app")!.appendChild(tt.el);
    tt.animate.bind(tt);
    tt.animate();
})();
