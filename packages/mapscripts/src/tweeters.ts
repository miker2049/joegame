// -*- lsp-enabled-clients: (deno-ls); -*-

/**
 * Utility functions for extracting tweeters from a conversation.
 */
export function getTweetersFromConvo(convo: [string, string][]): string[] {
    const tweeters = new Set<string>();
    for (const [_, t] of convo) {
        tweeters.add(t);
    }
    return Array.from(tweeters);
}

/*
 * Tweeters placed clockwise around the moore neighborhood starting from -1,0.
 *
 * 010
 * 0p0
 * 000
 *
 * 010
 * 0p0
 * 001
 *
 * 010
 * 1p0
 * 001
 *
 * 011
 * 1p0
 * 001
 *
 * 011
 * 1p0
 * 011
 *
 * 111
 * 1p0
 * 011
 *
 * 111
 * 1p1
 * 011
 *
 * 111
 * 1p1
 * 111
 */
export function createCrowd(
    x: number,
    y: number,
    n: number,
    space: number
): { x: number; y: number }[] {
    const offsets = [
        [0, -1],
        [1, 1],
        [-1, 0],
        [1, -1],
        [0, 1],
        [-1, -1],
        [1, 0],
        [-1, 1],
    ];
    const crowd = [];
    for (let i = 0; i < n; i++) {
        const [dx, dy] = offsets[i % offsets.length];
        const mult = Math.floor(i / offsets.length) + 1;
        crowd.push({ x: x + dx * space * mult, y: y + dy * space * mult });
    }
    return crowd;
}
