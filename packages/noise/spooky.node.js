export class Spooky {
    constructor(wasmMod) {
        this.mod = wasmMod;
        const _expts = this.mod.instance.exports;
        this.memory = _expts.memory;
        this.malloc = _expts.malloc;
        this.free = _expts.free;
        this.buffer = this.mod.instance.exports.memory.buffer;
        this.decoder = new TextDecoder();
        this.encoder = new TextEncoder();
    }

    hash64(msg, seed) {
        this._hashfunc(msg, seed, this.mod.instance.exports.spooky_64);
    }
    hash128(msg, seed) {
        this._hashfunc(msg, seed, this.mod.instance.exports.spooky_128);
    }

    _hashfunc(msg, seed, func) {
        const inn = this.str2ptr(msg);
        const out = func(inn.byteOffset, inn.length, BigInt(seed));

        const string = this.ptr2str(out, 32);
        console.log(string);
        return string;
    }

    // Convert a pointer from the wasm module to JavaScript string.
    ptr2str(ptr, length) {
        try {
            // The pointer is a multi byte character array encoded with utf-8.
            const array = new Uint8Array(this.memory.buffer, ptr, length);
            const string = this.decoder.decode(array);
            return string;
        } finally {
            // Free the memory sent to use from the WebAssembly instance.
            this.free(ptr);
        }
    }

    // Convert a JavaScript string to a pointer to multi byte character array
    str2ptr(string) {
        // Encode the string in utf-8.
        const bytes = this.encoder.encode(string);
        // Copy the string into memory allocated in the WebAssembly
        const ptr = this.malloc(bytes.byteLength);
        const buffer = new Uint8Array(
            this.memory.buffer,
            ptr,
            bytes.byteLength + 1
        );
        buffer.set(bytes);
        return buffer;
    }
}
