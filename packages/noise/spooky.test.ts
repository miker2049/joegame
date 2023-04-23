import test from "tape";
import { hash128 } from "./noise";
test("spooky hash works", (t) => {
    // s.hash128("hfsadasdasdas", 109);
    // s.hash128("h", 1011);
    // s.hash128("h", 109);
    // s.hash128("h", 109);
    // s.hash128("hey", 109);
    // s.hash128("hfy", 109);
    // s.hash128("hey", 109);

    // s.hash128("h", 109);
    // s.hash128("he", 109);
    // s.hash128("hel", 1090);
    // s.hash128("hl", 109);
    // s.hash128("hl", 109);
    // s.hash128("hlgg", 109);
    // s.hash128("hl123", 109);
    hash128(
        "hl123wkdjalkdjwadlkajdwawlkdjawbdlkawjbalckasjcnsalkjdnalksjcnaskclaskjc\nasdjlaskjdaslkjdaslkdjasdhlaksjdashlkdjaslkdjashldkasjdhlaskjdaslkdjaslbclaksjdhalskdjascblask",
        109
    );
    hash128(
        "hl123wkdjalkdjwadlkajdwawlkdjawbdlkawjbalckasjcnsalkjdnalksjcnaskclaskjc\nasdjlaskjdaslkjdaslkdjasdhlaksjdashlkdjaslkdjashldkasjdhlaskjdaslkdjaslbclaksjdhalskdjascblasf",
        109
    );

    hash128(
        "hl123wkdjalkdjwadlkajdwawlkdjawbdlkawjbalckasjcnsalkjdnalksjcnaskclaskjc\nasdjlaskjdaslkjdaslkdjasdhlaksjdashlkdjaslkdjashldkasjdhlaskjdaslkdjaslbclaksjdhalskdjascblasf",
        108
    );
    t.end();
});
