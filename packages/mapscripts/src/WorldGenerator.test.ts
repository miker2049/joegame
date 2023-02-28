import { describe, it, beforeEach } from "mocha";
import { expect } from "chai";
import {
    HashObjects,
    BinaryFilter,
    CliffSystem,
    FillSignal,
    GenericSignal,
    mapCliffPicture,
    Perlin,
    WangLayer,
    worldFromConfig,
    WorldGenerator,
} from "./WorldGenerator";
import { tiledMapFromFile } from "./utils-node";
import { writeFile, readFile } from "fs/promises";
import { createWriteStream } from "fs";
import { TiledMap } from "./TiledMap";
import { isNumber } from "mathjs";
import { createCanvas } from "canvas";

async function saveSignalToFile(
    filep: string,
    h: number,
    w: number,
    sig: GenericSignal
) {
    const cnv = createCanvas(w, h);
    const ctx = cnv.getContext("2d");
    await sig.renderToContext(w, h, ctx);
    await saveCanvasToFile(cnv, filep);
    return filep;
}

async function saveCanvasToFile(
    cnv: ReturnType<typeof createCanvas>,
    filep: string
) {
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
    const nn = 1000;
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
            await saveSignalToFile("perlin.png", 500, 500, sig, cnv);
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

    describe.skip("CliffSystem", function () {
        let cs: CliffSystem;
        let sig: Perlin;
        beforeEach(async function () {
            this.timeout(0);
            sig = new Perlin(0.01, 5, 69);
            cs = new CliffSystem("wg", sig, tm, []);
        });
    });

    describe.skip("WorldGenerator", function () {
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
        this.timeout(-1);
        it("Creates a mask which can render to an image", async function () {
            const sig = new Perlin(0.01, 5, 100202, [new BinaryFilter(0.7)]);
            (sig.filters[0] as BinaryFilter).setN(0.4);
            const wl = new WangLayer("grass", tm, sig);
            const filep = `wanglayer_mask.png`;
            expect(
                await saveSignalToFile(filep, nn, nn, wl.mask),
                "the resolved result of the saveCanvasToFile function"
            ).to.equal(filep, "the filepath of the saved image");
        });
    });

    describe("from config", function () {
        it("runs without error", async function () {
            const n = 500;
            const w = 100,
                h = 100;
            this.timeout(-1);
            const conf = JSON.parse(
                await readFile("src/world-settings.json", "utf-8")
            );

            const i = worldFromConfig(conf, tm);

            expect(i).to.not.be.undefined;
            const cnv = createCanvas(w, h);
            const ctx = cnv.getContext("2d");
            if (ctx) {
                await mapCliffPicture(i, 0, 0, w, h, ctx, conf);
                await saveCanvasToFile(cnv, "conf-test.png");
            }
        });
    });

    describe("HashObjects", function () {
        it("runs without error", async function () {
            const n = 500;
            const w = 100,
                h = 100;
            this.timeout(-1);
            const conf = JSON.parse(
                await readFile("src/world-settings.json", "utf-8")
            );

            const i = worldFromConfig(conf, tm);

            const ho = new HashObjects(i, conf);
            console.log(ho.getXYObjects(32, 43));
        });
    });
});
