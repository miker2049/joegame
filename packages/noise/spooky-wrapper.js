export class Spooky {
    constructor(wasmMod) {
        this.expts = wasmMod.exports;
        this.memory = this.expts.memory;
        this.malloc = this.expts.malloc;
        this.free = this.expts.free;
        this.decoder = new TextDecoder();
        this.encoder = new TextEncoder();
    }

    hash64(msg, seed) {
        return this._hashfunc(msg, seed, this.expts.spooky_64);
    }
    hash128(msg, seed) {
        return this._hashfunc(msg, seed, this.expts.spooky_128);
    }

    _hashfunc(msg, seed, func) {
        const inn = this.str2ptr(msg);
        const out = func(inn.byteOffset, inn.length, BigInt(seed));

        const string = this.ptr2str(out, 32);
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
            bytes.byteLength
        );
        buffer.set(bytes);
        return buffer;
    }
}
