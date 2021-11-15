const std = @import("std");

pub fn build(b: *std.build.Builder) void {
    // Standard target options allows the person running `zig build` to choose
    // what target to build for. Here we do not override the defaults, which
    // means any target is allowed, and the default is native. Other options
    // for restricting supported target set are available.

    // Standard release options allow the person running `zig build` to select
    // between Debug, ReleaseSafe, ReleaseFast, and ReleaseSmall.
    const mode = b.standardReleaseOptions();

    const fluidlite = b.addSharedLibrary("fluidlite",  "src/main.zig",.unversioned);
    fluidlite.linkLibC();

    fluidlite.addCSourceFiles(&[_][]const u8{
        "Fluidlite/src/walloc.c",
        "Fluidlite/src/fluid_init.c",
        "Fluidlite/src/fluid_chan.c",
        "Fluidlite/src/fluid_chorus.c",
        "Fluidlite/src/fluid_conv.c",
        "Fluidlite/src/fluid_defsfont.c",
        "Fluidlite/src/fluid_dsp_float.c",
        "Fluidlite/src/fluid_gen.c",
        "Fluidlite/src/fluid_hash.c",
        "Fluidlite/src/fluid_list.c",
        "Fluidlite/src/fluid_mod.c",
        "Fluidlite/src/fluid_ramsfont.c",
        "Fluidlite/src/fluid_rev.c",
        "Fluidlite/src/fluid_settings.c",
        "Fluidlite/src/fluid_synth.c",
        "Fluidlite/src/fluid_sys.c",
        "Fluidlite/src/fluid_tuning.c",
        "Fluidlite/src/fluid_voice.c",
        "Fluidlite/stb/stb_vorbis.c",
        }, &[_][]const u8{
        "--std=c99",
        "-DSF3_SUPPORT=2",
        "-fno-stack-protector",
    });
    fluidlite.addIncludeDir("Fluidlite/include");
    fluidlite.addIncludeDir("Fluidlite/src");
    fluidlite.addIncludeDir("Fluidlite/stb");


    fluidlite.setTarget(.{.cpu_arch=.wasm32, .os_tag=.freestanding });
    // exe.setTarget(target);
    fluidlite.setBuildMode(mode);
    fluidlite.install();

    // const run_cmd = exe.run();
    // run_cmd.step.dependOn(b.getInstallStep());
    // if (b.args) |args| {
    //     run_cmd.addArgs(args);
    // }

    // const run_step = b.step("run", "Run the app");
    // run_step.dependOn(&run_cmd.step);
}
