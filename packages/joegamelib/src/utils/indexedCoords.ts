export function indexToCoords(n: number, w: number): [number,number] {
    return [
        n % w,
        Math.floor(n / w)
    ]
}

export function coordsToIndex(x: number, y: number, width: number): number {
    return (y * width) + x
}
