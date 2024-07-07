const std = @import("std");
const game = @import("./game.zig");
const utils = @import("./utils.zig");
const curses = @import("./curses-app.zig");
const row_raw_data = @embedFile("row_1151.bin");
const config = @embedFile("config.json");
// const row_data: []const i16 = u8SliceToi16Slice(row_raw_data);

const Message = struct { index: usize, message: []u8 };
const Config = struct { elevations: []usize, messages: []Message };

const row_data = utils.u8SliceToi16Slice(row_raw_data);

pub fn main() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    const allocator = arena.allocator();
    // defer arena.deinit();

    var config_file = try std.fs.cwd().openFile("./config.json", .{});
    const config_str = try config_file.readToEndAlloc(allocator, 32_000_000);
    defer config_file.close();
    const c = try std.json.parseFromSliceLeaky(game.GameConfig, allocator, config_str, .{});

    // stdout is for the actual output of your application, for example if you
    // are implementing gzip, then only the compressed bytes should be sent to
    // stdout, not any debugging messages.
    const stdout_file = std.io.getStdOut().writer();
    var bw = std.io.bufferedWriter(stdout_file);
    const stdout = bw.writer();
    _ = stdout;
    var state: game.GameState = try game.GameState.init(c, &std.heap.page_allocator);
    defer state.free();
    // const a = game.alloc2d(&allocator, 10, 10);
    // state.move_view(12, 12);
    try state.render();
    try curses.setup_ncurses();

    // Wait for a keypress
    try curses.main_loop(&state);
    // End ncurses mode
    // const key = ncurses.getch();

    // _ = ncurses.endwin();
    // // const res = try state.render(&allocator);
    // try stdout.print("{}", .{key});

    // state.move_view(12, 12);
    // try stdout.print("game err: {any}\n", .{a});

    // try stdout.print("Image data size: {}\n", .{row_data.len});
    // try stdout.print("Image data size: {}, {}\n", .{ state.get_elev(30), state.min });
    // try stdout.print("Run `zig build test` to run the tests.\n", .{});

    // try cursesMain();

    try bw.flush(); // don't forget to flush!
    _ = arena.reset(std.heap.ArenaAllocator.ResetMode.retain_capacity);
}

test "simple test" {
    var list = std.ArrayList(i32).init(std.testing.allocator);
    defer list.deinit(); // try commenting this out and see if zig detects the memory leak!
    try list.append(42);
    try std.testing.expectEqual(@as(i32, 43), list.pop());
}
