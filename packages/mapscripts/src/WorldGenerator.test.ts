import { describe, it, beforeEach } from "mocha";
import { expect } from "chai";
import {
    BinaryFilter,
    CliffSystem,
    FillSignal,
    GenericSignal,
    Perlin,
    WangLayer,
    WorldGenerator,
} from "./WorldGenerator";
import { tiledMapFromFile } from "./utils-node";
import { writeFile } from "fs/promises";
import { createWriteStream } from "fs";
import { TiledMap } from "./TiledMap";
import { isNumber } from "mathjs";
import { createCanvas } from "canvas";

async function saveCanvasToFile(
    filep: string,
    h: number,
    w: number,
    sig: GenericSignal,
    cnv: ReturnType<typeof createCanvas>
) {
    const ctx = cnv.getContext("2d");
    await sig.renderToContext(w, h, ctx);
    const st = cnv.createPNGStream();
    const out = createWriteStream(filep);
    await new Promise((res) => {
        st.pipe(out);
        out.on("finish", () => {
            res(undefined);
        });
    });
    return filep;
}

describe("main", function () {
    const nn = 10;
    let tm: TiledMap, wg: WorldGenerator;
    let cnv: ReturnType<typeof createCanvas>,
        ctx: ReturnType<typeof cnv.getContext>;
    beforeEach(async function () {
        tm = await tiledMapFromFile(
            "../../assets/maps/desert/desert-stamps2.json"
        );
        wg = new WorldGenerator(tm.getConf());
        cnv = createCanvas(nn, nn);
        ctx = cnv.getContext("2d");
    });

    describe("Perlin signal", function () {
        it("generates a signal", function () {
            let sig = new Perlin(0.01, 5, 69);
            expect(sig.get(23, 20)).to.satisfy((l) => isNumber(l));
            const arr = Array(100)
                .fill(0)
                .map((_, idx) => sig.get(3, idx));
            // arr.forEach((v) => console.log(v));
            // expect(arr.every((v) => arr.every((vv) => v !== vv))).to.be.true;
        });

        it("write to file", async function () {
            this.timeout(0);
            let cnv = createCanvas(500, 500);
            let sig = new Perlin(0.01, 5, 69);
            sig.filters.push(new BinaryFilter(0.33, 420));
            await saveCanvasToFile("perlin.png", 500, 500, sig, cnv);
        });
    });

    describe("Fill signal", function () {
        it("generates a signal", function () {
            let sig = new FillSignal();
            expect(sig.get(23, 20)).to.equal(1);
            expect(sig.get(24, 20)).to.equal(1);
            expect(sig.get(25, 20)).to.equal(1);
            expect(sig.get(26, 21)).to.equal(1);
            // arr.forEach((v) => console.log(v));
            // expect(arr.every((v) => arr.every((vv) => v !== vv))).to.be.true;
        });
    });

    describe("CliffSystem", function () {
        let cs: CliffSystem;
        let sig: Perlin;
        beforeEach(async function () {
            this.timeout(0);
            sig = new Perlin(0.01, 5, 69);
            cs = new CliffSystem("wg", sig, 12, tm);
        });

        it("Get tile from cliffs, depending on divs", () => {
            wg.setCliffDivs(12);
            for (let jj = 0; jj < 12; jj++) {
                const lg = cs.getLayerGroup(jj);
                const c = lg ? lg[0].getXYTiles(2, 3) : undefined;
                expect(c).is.not.undefined;
            }
            wg.setCliffDivs(1);
            for (let jj = 0; jj < 1; jj++) {
                const lg = cs.getLayerGroup(jj);
                const c = lg ? lg[0].getXYTiles(2, 3) : undefined;
                expect(c).is.not.undefined;
            }
        });
        it.skip("Can generate distinct layers", () => {
            const lgs = cs.getAllLayerGrids(0, 0, 20, 20);
            expect(
                lgs.every((v) => lgs.every((vv) => !v.isSame(vv))),
                "each layer is different from the other.."
            ).to.be.true;
        });
        it("Trying to pick non existent tile from cliff just gives undefined", () => {
            wg.setCliffDivs(12);
            const c = cs.getLayerGroup(12);
            expect(c).to.be.undefined;
        });
        it("Creates a proper div mask", async function () {
            cs.setDivs(8);
            const sig = cs.getLayerGroup(4)[0]?.mask;
            expect(sig).to.not.be.undefined;
            if (sig) saveCanvasToFile("tst_single.png", nn, nn, sig, cnv);
        });
        it("Creates proper div masks", function (done) {
            this.timeout(0);
            Promise.all(
                Array(12)
                    .fill(0)
                    .map((_, idx) => {
                        const cnv = createCanvas(nn, nn);
                        const ctx = cnv.getContext("2d");
                        const sig = cs.getLayerGroup(idx)[0]?.mask;
                        if (!sig) return undefined;
                        return saveCanvasToFile(
                            `tst_${idx}.png`,
                            nn,
                            nn,
                            sig,
                            cnv
                        );
                    })
            ).then((_) => done());
        });
    });

    describe("WorldGenerator", function () {
        this.timeout(0);
        it("Gets instantiated ok", function () {
            expect(wg, "World generator has been instantiated.").is.instanceOf(
                WorldGenerator
            );
        });

        it("Can write map files", () => {
            const mm = wg.getMap(1000, 1000, nn, nn);
            expect(mm.layers[0].data.length).to.equal(nn * 4 * nn * 4);
            return writeFile(
                "../../assets/maps/desert/wg-test.json",
                JSON.stringify(mm)
            );
        });
    });

    describe("WangLayer", function () {
        it("Creates a mask which can render to an image", async function () {
            const sig = new Perlin(0.01, 5, 10002, [new BinaryFilter(0.7, 99)]);
            (sig.filters[0] as BinaryFilter).setN(0.4);
            const wl = new WangLayer("grass", tm, sig);
            const filep = `wanglayer_mask.png`;
            expect(
                await saveCanvasToFile(filep, nn, nn, wl.mask, cnv),
                "the resolved result of the saveCanvasToFile function"
            ).to.equal(filep, "the filepath of the saved image");
        });
    });
});
