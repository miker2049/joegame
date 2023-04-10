import { assertEquals } from "https://deno.land/std@0.182.0/testing/asserts.ts";
import { createCrowd } from "./tweeters.ts";

const { test } = Deno;

test("createCrowd", () => {
    const crowd = createCrowd(25, 25, 3, 16);

    assertEquals(crowd.length, 3);
    // [0,-1]
    assertEquals(crowd[0].y, 25 - 16);
    assertEquals(crowd[0].x, 25);
    // 1,1
    assertEquals(crowd[1].x, 25 + 16);
    assertEquals(crowd[1].y, 25 + 16);
});
