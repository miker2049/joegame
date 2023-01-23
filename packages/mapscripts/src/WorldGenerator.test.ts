import { describe, it, beforeEach } from "mocha";
import { expect } from "chai";
import {
    BinaryFilter,
    CliffSystem,
    GenericSignal,
    Perlin,
    SnapFilter,
    WangLayer,
    withFilter,
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
            res(null);
        });
    });
}

describe("main", function () {
    const nn = 300;
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

    describe("CliffSystem", function () {
        let cs: CliffSystem;
        let sig: Perlin;
        beforeEach(async function () {
            this.timeout(0);
            sig = new Perlin(0.01, 5, 69);
            cs = new CliffSystem("cliffs", sig, 12, tm, "cliffs");
        });

        it("Creates proper div masks", function (done) {
            this.timeout(0);
            Promise.all(
                Array(12)
                    .fill(0)
                    .map((_, idx) => {
                        const cnv = createCanvas(nn, nn);
                        const ctx = cnv.getContext("2d");
                        const sig = cs.getAltitudeLayer(idx)?.mask;
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
        it("Get tile from cliffs, depending on divs", () => {
            wg.setCliffDivs(12);
            for (let jj = 0; jj < 12; jj++) {
                const c = wg.cliffSystem.getAltitudeLayer(jj)?.getXYTiles(2, 3);
                expect(c).is.not.undefined;
            }
            wg.setCliffDivs(1);
            for (let jj = 0; jj < 1; jj++) {
                const c = wg.cliffSystem.getAltitudeLayer(jj)?.getXYTiles(2, 3);
                expect(c).is.not.undefined;
            }
        });
        it("Can generate distinct layers", () => {
            const lgs = wg.cliffSystem.getAllLayerGrids(0, 0, 20, 20);
            // tt.assert(
            //     lgs.every((v) => lgs.every((vv) => !v.isSame(vv))),
            //     "Each layer is distinct"
            // );
            // console.log(lgs);
        });
        it("Trying to pick non existent tile from cliff just gives undefined", () => {
            wg.setCliffDivs(12);
            const c = wg.cliffSystem.getAltitudeLayer(12);
            expect(c).to.be.undefined;
        });

        it("Can write map files", () => {
            const mm = wg.getMap(1000, 1000, 30, 30);
            expect(mm.layers[0].data.length).to.equal(30 * 4 * 30 * 4);
            return writeFile(
                "../../assets/maps/desert/wg-test.json",
                JSON.stringify(mm)
            );
        });
    });

    describe("WangLayer", function () {
        it("Creates a mask which can render to an image", async function () {
            const sig = new Perlin(0.01, 5, 10002, [new BinaryFilter(0.7, 99)]);
            const wl = new WangLayer("grass", tm, sig);
            await wl.mask.renderToContext(500, 500, ctx);
            const st = cnv.createPNGStream();
            const out = createWriteStream(`wanglayer_mask.png`);
            await new Promise((res) => {
                st.pipe(out);
                out.on("finish", () => {
                    res(null);
                });
            });
        });
    });
});
