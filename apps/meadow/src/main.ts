import { Meadow } from './meadow'
import './style.css'

/*
 * Colors
 */
const COLOR = 0xFCE920
const COLORS_N = 16

function hexToRGB(n: number) {
    return {
        r: n >> 16,
        g: (n >> 8) & 255,
        b: n & 255
    }
}


// 55 97 56

const colors = Array(COLORS_N).fill(0).map((_, idx) => {
    const l = interpolate(56, 0, idx, COLORS_N)
    return `hsl(55,97%,${Math.floor(l)}%)`
})

function interpolate(startValue: number, endValue: number, stepNumber: number, lastStepNumber: number): number {
    return (endValue - startValue) * stepNumber / lastStepNumber + startValue;
}

/* accepts parameters
 * r, g, b
 *  ty https://stackoverflow.com/a/17243070
*/
function RGBtoHSV(r: number, g: number, b: number) {
    var max = Math.max(r, g, b), min = Math.min(r, g, b),
        d = max - min,
        h,
        s = (max === 0 ? 0 : d / max),
        v = max / 255;

    switch (max) {
        case min: h = 0; break;
        case r: h = (g - b) + d * (g < b ? 6 : 0); h /= 6 * d; break;
        case g: h = (b - r) + d * 2; h /= 6 * d; break;
        case b: h = (r - g) + d * 4; h /= 6 * d; break;
    }

    return {
        h: h,
        s: s,
        v: v
    };
}

/* accepts parameters
 * h, s, v
 *  ty https://stackoverflow.com/a/17243070
*/
function HSVtoRGB(h: number, s: number, v: number) {
    let r: number, g: number, b: number, i: number,
        f: number, p: number, q: number, t: number;

    i = Math.floor(h * 6);
    f = h * 6 - i;
    p = v * (1 - s);
    q = v * (1 - f * s);
    t = v * (1 - (1 - f) * s);

    switch (i % 6) {
        case 0: r = v, g = t, b = p; break;
        case 1: r = q, g = v, b = p; break;
        case 2: r = p, g = v, b = t; break;
        case 3: r = p, g = q, b = v; break;
        case 4: r = t, g = p, b = v; break;
        case 5: r = v, g = p, b = q; break;
    }

    return {
        r: Math.round(r! * 255),
        g: Math.round(g! * 255),
        b: Math.round(b! * 255)
    };
}
/*
 * Table work
 */
const TICK = 100
const ROW_N = 8
const COL_N = 16
const app = document.querySelector<HTMLDivElement>('#app')!

const NOOP = () => undefined
interface Grid<T>{
    items: T[]
    getItem: (x: number, y: number)=> T
    width: number
    height: number
}

class TableGrid implements Grid<HTMLTableCellElement>{
    el: HTMLTableElement
    items: HTMLTableCellElement[]
    width: number
    height: number
    constructor(w: number, h: number) {
        this.width = w
        this.height = h
        this.el = document.createElement("table")
        const rows = Array(h).fill(0).map(_ => document.createElement("tr"))
        this.cellCb.bind(this)
        this.items = Array(h * w).fill(0).map((_, idx) => {
            const el = document.createElement("td")
            el.onclick = () => this.cellCb(idx)
            return el
        })

        rows.forEach((row, idx) => {
            for (let i = 0; i < COL_N; i++) {
                row.appendChild(this.items[(idx * COL_N) + i])
            }
            this.el.appendChild(row)
        })
    }

    getItem(x: number, y: number) {
        return this.items[(y * this.width) + x]
    }

    setCellColor(x: number, y: number, color: string) {
        this.getItem(x, y).style.backgroundColor = color
    }

    cellCb(c: number) {

    }
}

interface MeadowRender<C>  {
    mp: Meadow
    setColor: (x:number, y: number, color: C) => void
    activeColor: C
    inActiveColor:  C
    posColor: C
}

type GenericColor ={r: number, g: number, b: number}

class MeadowTableGeneric_ implements Grid<{color: GenericColor}>, MeadowRender<GenericColor> {
    mp: Meadow
    activeColor: GenericColor
    inActiveColor: GenericColor
    posColor: GenericColor
    items: {color: GenericColor}
    width: number
    height: number
    constructor(w: number, h: number) {
        this.mp = new Meadow(w, h, NOOP, NOOP)
        this.width = w
        this.height = h
        this.inActiveColor = {r: 0, g: 0, b: 0}
        this.activeColor = {r: 129, g: 129, b: 129}
        this.posColor = {r: 255, g: 255, b: 255}
        this.items = Array(w*h).fill(0).map(_=>{return {color: this.inActiveColor}})
        this.animate.bind(this)
    }
    cellCb(i) {
        const row = Math.floor(i / this.width)
        this.mp.setCount(row, i % this.width)
        this.renderRow(row)
    }
    renderRow(y){
            const count = this.mp.rows[y].count
            const pos = this.mp.rows[y].pos
            // console.log(`y: ${y} count: ${count} pos ${pos} height: ${this.height}`)
            for (let i = this.width-1; i >= 0; --i) {
                if(i<=count)
                    this.getItem(i, y).style.backgroundColor = this.activeColor
                else
                    this.getItem(i, y).style.backgroundColor = 'black'

            }
            this.getItem(pos, y).style.backgroundColor = this.posColor
    }
    animate() {
        for (let y = 0; y < this.height; y++) {
            this.renderRow(y)
        }
        this.mp.tick()
        this.animate.bind(this)
        setTimeout(() =>
            requestAnimationFrame(() => this.animate()), 500)
    }

    setColor(x: number, y: number, color: string){
        this.getItem(x, y).style.backgroundColor = color
    }
}
class MeadowTableGeneric implements MeadowRender<{color:GenericColor}, GenericColor> {
    mp: Meadow
    activeColor: string
    inActiveColor: string
    posColor: string
    constructor(w: number, h: number) {
        this.mp = new Meadow(w, h, NOOP, NOOP)
        this.inActiveColor = 'black'
        this.activeColor = 'hsl(55,96%,40%)'
        this.posColor = 'hsl(55,96%,56%)'
        this.animate.bind(this)
    }
    cellCb(i) {
        const row = Math.floor(i / this.width)
        this.mp.setCount(row, i % this.width)
        this.renderRow(row)
    }
    renderRow(y){
            const count = this.mp.rows[y].count
            const pos = this.mp.rows[y].pos
            // console.log(`y: ${y} count: ${count} pos ${pos} height: ${this.height}`)
            for (let i = this.width-1; i >= 0; --i) {
                if(i<=count)
                    this.getItem(i, y).style.backgroundColor = this.activeColor
                else
                    this.getItem(i, y).style.backgroundColor = 'black'

            }
            this.getItem(pos, y).style.backgroundColor = this.posColor
    }
    animate() {
        for (let y = 0; y < this.height; y++) {
            this.renderRow(y)
        }
        this.mp.tick()
        this.animate.bind(this)
        setTimeout(() =>
            requestAnimationFrame(() => this.animate()), 500)
    }

    setColor(x: number, y: number, color: string){
        this.getItem(x, y).style.backgroundColor = color
    }
}


const tt = new MeadowHTMLTable(16, 8)
tt.mp.toggleLink(0, 1)
tt.mp.toggleLink(0, 2)
tt.mp.toggleLink(2, 3)


document.querySelector('#app')!.appendChild(tt.el)
tt.animate.bind(tt)
tt.animate()
