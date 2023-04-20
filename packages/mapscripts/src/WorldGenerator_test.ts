// -*- lsp-enabled-clients: (deno-ls); -*-
import {
    describe,
    it,
    beforeEach,
} from "https://deno.land/std@0.182.0/testing/bdd.ts";
import { expect } from "https://cdn.skypack.dev/chai@4.3.4?dts";
import {
    HashObjects,
    BinaryFilter,
    CliffSystem,
    FillSignal,
    GenericSignal,
    mapCliffPicture,
    Perlin,
    WangLayer,
    cliffSystemFromConfig,
    WorldGenerator,
} from "./WorldGenerator.ts";
import { tiledMapFromFile } from "./utils-node.ts";
import { TiledMap } from "./TiledMap.ts";
import { createCanvas } from "https://deno.land/x/canvas@v1.4.1/mod.ts";
import { ITileLayer } from "../../joegamelib/src/types/TiledRawJson.d.ts";
import { applyObjects } from "./saturator.ts";
const readFile = Deno.readTextFile;

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
    try {
        await Deno.writeFile(filep, cnv.toBuffer());
        return filep;
    } catch (e) {
        console.log(e);
        throw e;
    }
}

describe("main", function () {
    const nn = 1000;
    let tm: TiledMap, wg: WorldGenerator;
    let cnv: ReturnType<typeof createCanvas>;
    beforeEach(async function () {
        tm = await tiledMapFromFile("../../assets/maps/desert-stamps2.json");
        const conf = JSON.parse(await readFile("src/world-settings.json"));
        wg = new WorldGenerator(tm, conf);
        cnv = createCanvas(nn, nn);
    });

    describe("Perlin signal", function () {
        it("generates a signal", function () {
            let sig = new Perlin(0.01, 5, 69);
            expect(sig.get(23, 20)).to.satisfy(
                (l: unknown) => typeof l === "number"
            );
            const arr = Array(100)
                .fill(0)
                .map((_, idx) => sig.get(3, idx));
            // arr.forEach((v) => console.log(v));
            // expect(arr.every((v) => arr.every((vv) => v !== vv))).to.be.true;
        });

        it("write to file", async function () {
            // this.timeout(0);
            let cnv = createCanvas(500, 500);
            let sig = new Perlin(0.01, 5, 69);
            sig.filters.push(new BinaryFilter(0.33));
            await saveSignalToFile("perlin.png", 500, 500, sig);
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

    describe.ignore("CliffSystem", function () {
        let cs: CliffSystem;
        let sig: Perlin;
        beforeEach(async function () {
            // this.timeout(0);
            sig = new Perlin(0.01, 5, 69);
            cs = new CliffSystem("wg", sig, tm, []);
        });
    });

    describe.ignore("WorldGenerator", function () {
        // this.timeout(0);
        it("Gets instantiated ok", function () {
            expect(wg, "World generator has been instantiated.").is.instanceOf(
                WorldGenerator
            );
        });

        it("Can write map files", async function () {
            const mm = await wg.getMap(1000, 1000, nn, nn);
            let lay = mm.layers[0];
            expect(TiledMap.isTileLayer(lay)).to.be.true;
            lay = lay as ITileLayer;
            expect(lay.data.length).to.equal(nn * 4 * nn * 4);
            return Deno.writeTextFile(
                "../../assets/maps/wg-test.json",
                JSON.stringify(mm)
            );
        });
    });

    describe("WangLayer", function () {
        // this.timeout(-1);
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
            // this.timeout(-1);
            const conf = JSON.parse(await readFile("src/world-settings.json"));

            const i = cliffSystemFromConfig(conf, tm);

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
            // this.timeout(-1);
            const conf = JSON.parse(await readFile("src/world-settings.json"));

            const i = cliffSystemFromConfig(conf, tm);

            const ho = new HashObjects(i, conf);
            const objs = await ho.getXYObjects(32, 43, 32, 43);
            await applyObjects(tm, objs, "objs");
        });
    });
});
