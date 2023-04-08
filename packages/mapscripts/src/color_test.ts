// -*- lsp-enabled-clients: (deno-ls); -*-
import { describe, it } from "https://deno.land/std@0.182.0/testing/bdd.ts";
import { expect } from "https://cdn.skypack.dev/chai@4.3.4?dts";
import { hsvToRgb, rgbToHsv } from "./color.ts";

describe("color functions", function () {
    it("is idempotent", function () {
        const c = { r: 124, g: 23, b: 134 };
        expect(hsvToRgb(rgbToHsv(c))).to.deep.equal(c);
    });
});
