import { expect } from "chai";
import { describe, it } from "mocha";
import { hsvToRgb, rgbToHsv } from "./color";

describe("color functions", function () {
    it("is idempotent", function () {
        const c = { r: 124, g: 23, b: 134 };
        expect(hsvToRgb(rgbToHsv(c))).to.deep.equal(c);
    });
});
