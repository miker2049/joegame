const std = @import("std");
const fluid = @cImport({
    @cInclude("fluidlite.h");
});

const SAMPLE_RATE = 44100;
const SAMPLE_SIZE = @sizeOf(f32);
const NUM_FRAMES = SAMPLE_RATE;
const NUM_CHANNELS = 2;
const NUM_SAMPLES = NUM_FRAMES * NUM_CHANNELS;

// export fn get_settings() c_int {
//     _ = fluid.new_fluid_settings();

//     return  1;
// }

export fn sin(n: f64) f64 {
    return std.math.sin(n);
}
export fn cos(n: f64) f64 {
    return std.math.cos(n);
}
export fn pow(n: f64, p: f64) f64 {
    return std.math.pow(f64, n, p);
}
// export fn abs(n: i32) i32 {
//     const out: i32 = std.math.absInt(n);
//     return out;
// }
export fn fabs(n: f64) f64 {
    return std.math.absFloat(n);
}

export fn memcpy(dest: [*]u8, src: [*]u8, count: usize) [*]u8 {
    @memcpy(dest, src, count);
    return dest;
}

export fn floor(n: f64) f64 {
    return std.math.floor(n);
}
export fn add() c_int {
    return 2;
}
