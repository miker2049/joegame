export class Spooky {
    constructor(wasmMod) {
        this.mod = wasmMod;
        this.buffer = this.mod.instance.exports.memory.buffer;
        this.decoder = new TextDecoder();
        this.encoder = new TextEncoder();
    }
    hash128(msg, seed) {
        const inn = new Uint8Array(this.buffer, 0, msg.length);
        this.encoder.encodeInto(msg, inn);
        const out = this.mod.instance.exports.spooky_128(
            inn.byteOffset,
            inn.length,
            BigInt(seed)
        );

        const boff = new Uint8Array(this.buffer, out, 32);
        const string = this.decoder.decode(boff);
        console.log(string);
        return string;
    }

    hash64(msg, seed) {
        const inn = new Uint8Array(this.buffer, 0, msg.length);
        this.encoder.encodeInto(msg, inn);
        const out = this.mod.instance.exports.spooky_64(
            arr.byteOffset,
            arr.length,
            BigInt(seed)
        );

        const boff = new Uint8Array(this.buffer, out, 16);
        const string = this.decoder.decode(boff);
        return string;
    }
}
