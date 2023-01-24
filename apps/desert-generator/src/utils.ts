export async function toDataURL(
    cb: (
        w: number,
        h: number,
        ctx: OffscreenCanvasRenderingContext2D
    ) => Promise<void>,
    w: number,
    h: number
): Promise<string> {
    const cnv = new OffscreenCanvas(w, h);
    const ctx = cnv.getContext("2d");
    if (!ctx) throw Error("Couldn't create canvas when making picture");
    await cb(w, h, ctx);
    const blob = await cnv.convertToBlob({ type: "image/png" });
    const imgs = URL.createObjectURL(blob);
    return imgs;
}
