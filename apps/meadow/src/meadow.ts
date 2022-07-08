
interface row {
    count: number
    links: number[]
    pos: number
    evCb: (n: number) => void
    stepCb: (n: number) => void
}

export class Meadow {
    rows: row[]

    /*
     * The width is maximum count.
     * The height is number of rows
     */
    constructor(private width: number,
                private height: number,
                evCb: (n: number) => void,
                stepCb: (n: number) => void) {
        this.rows = []
        Array(this.height).fill(0).forEach(_ => this.rows.push({
            count: this.width - 1,
            links: [],
            pos: this.width - 1,
            evCb: evCb,
            stepCb: stepCb
        }))
    }

    step(r: number) {
        let cell = this.rows[r]
        --cell.pos
        if (cell.pos === -1) {
            cell.pos = cell.count
        } else if (cell.pos === 0) {
            cell.links.forEach(l => this.step(l))
            console.log('event')
            cell.evCb(r)
        }
        cell.stepCb(r)
    }

    tick() {
        this.step(0)
    }


    /*
     * Toggles a row to have a link. Has it if not set,
     * turned off if not.
     */
    toggleLink(row: number, link: number) {
        if (row === link) return
        const found = this.rows[row].links.indexOf(link)
        if (found != -1) {
            this.rows[row].links = this.rows[row].links.filter(l => l != link)
        } else {
            this.rows[row].links.push(link)
        }
    }

    /*
     * Sets the count of a given row
     */
    setCount(row: number, count: number) {
        this.rows[row].count = count
        if (this.rows[row].pos > count)
            this.rows[row].pos = count
    }

}
