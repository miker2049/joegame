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

export fn add() c_int {
    return  2;
}
