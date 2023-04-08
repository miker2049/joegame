// -*- lsp-enabled-clients: (deno-ls); -*-
import { getTweetConvo } from "./utils.ts";
Deno.test("getConvos", () => {
    getTweetConvo(180);
});
