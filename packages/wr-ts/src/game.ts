import { setup, createActor, assign } from "xstate";

export function createGame() {
    const machine = setup({
        types: {
            context: { count: 0 },
            events: {} as { type: "toggle" },
        },
    }).createMachine({
        id: "toggle",
        initial: "active",
        context: { count: 0 },
        states: {
            active: {
                entry: assign({
                    count: ({ context }) => context.count + 1,
                }),
                on: {
                    toggle: { target: "inactive" },
                },
            },
            inactive: {
                on: {
                    toggle: { target: "active" },
                },
            },
        },
    });
}
