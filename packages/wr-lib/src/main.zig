const std = @import("std");
const game = @import("./game.zig");
const row_raw_data: []const u8 = @embedFile("row_1151.bin");
// const row_data: []const i16 = u8SliceToi16Slice(row_raw_data);

const row_data = u8SliceToi16Slice(row_raw_data);

const ncurses = @cImport({
    @cInclude("ncurses.h");
});

fn setup_ncurses() !void {
    const initResult = ncurses.initscr();
    if (initResult == ncurses.ERR) {
        std.debug.print("Error initializing ncurses\n", .{});
        return;
    }
    _ = ncurses.keypad(ncurses.stdscr, true);
}

fn render_curses(state: *game.GameState) !void {
    for (0..@intCast(state.view.height)) |y| {
        for (0..@intCast(state.view.width)) |x| {
            const idx = state.get_index(x, y);
            const xx: c_int = @intCast(x);
            const yy: c_int = @intCast(34 - y);
            _ = ncurses.mvaddch(yy, xx, state.buffer[idx]);
        }
    }
    _ = ncurses.refresh(); // Refresh the screen
}

fn mv_render_curses(state: *game.GameState, xcoord: usize, ycoord: usize) !void {
    state.move_view(@intCast(xcoord), @intCast(ycoord));
    try state.render();
    render_curses(state);
}

fn main_loop(state: *game.GameState) !void {
    var active = true;
    while (active) {
        try render_curses(state);
        const ch = ncurses.getch();
        active = switch (ch) {
            // up
            259 => blk: {
                _ = state.move_view(state.view.x, state.view.y + 1);
                break :blk true;
            },
            //down
            258 => blk: {
                _ = state.move_view(state.view.x, state.view.y - 1);
                break :blk true;
            },
            // left
            260 => blk: {
                _ = state.move_view(state.view.x - 1, state.view.y);
                break :blk true;
            },
            //right
            261 => blk: {
                _ = state.move_view(state.view.x + 1, state.view.y);
                break :blk true;
            },
            else => false,
        };
    }
    _ = ncurses.endwin();
}

pub fn u8SliceToi16Slice(bytes: []const u8) []const i16 {
    if (bytes.len % 2 != 0) {
        @compileError("Data size must be a multiple of 2");
    }
    return @as([*]const i16, @alignCast(@ptrCast(bytes.ptr)))[0 .. bytes.len / 2];
}

pub fn writeu32Str(bytes: []u32, allocator: std.mem.Allocator) !*[]u8 {
    var buf = try allocator.alloc(u8, bytes.len * 4);
    for (bytes) |ele| {
        _ = try std.unicode.utf8Encode(@intCast(ele), buf);
    }
    return &buf;
}

pub fn writeu21Str(nums: []const u21, allocator: *std.mem.Allocator) ![]u8 {
    // The output buffer. Start with an empty array and grow as needed.
    var buffer = try allocator.alloc(u8, 0);

    for (nums) |num| {
        var num_buffer: [50]u8 = undefined; // Temp buffer for number
        // Format the current number as a string
        // const num_str = try std.fmt.bufPrint(&num_buffer, "{}", .{num});
        const str_len = try std.unicode.utf8Encode(@intCast(num), &num_buffer);
        // Reallocate buffer to fit new string size
        buffer = try allocator.realloc(buffer, buffer.len + str_len + 1); // +1 for optional separator
        std.mem.copy(u8, buffer[buffer.len - str_len - 1 ..], num_buffer[0..str_len]); // Copy the number string into the buffer
        buffer[buffer.len - 1] = ' '; // Put some separator, a space in this case
    }

    if (buffer.len > 0) buffer[buffer.len - 1] = 0; // Null terminate the string
    return buffer;
}

pub fn main() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    const allocator = arena.allocator();

    // stdout is for the actual output of your application, for example if you
    // are implementing gzip, then only the compressed bytes should be sent to
    // stdout, not any debugging messages.
    const stdout_file = std.io.getStdOut().writer();
    var bw = std.io.bufferedWriter(stdout_file);
    const stdout = bw.writer();
    _ = stdout;
    var state: game.GameState = try game.GameState.init(300, 20, 80, 35, row_data, &allocator);
    defer state.free();
    // const a = game.alloc2d(&allocator, 10, 10);
    // state.move_view(12, 12);
    try state.render();
    try setup_ncurses();

    // Wait for a keypress
    try main_loop(&state);
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

}

test "simple test" {
    var list = std.ArrayList(i32).init(std.testing.allocator);
    defer list.deinit(); // try commenting this out and see if zig detects the memory leak!
    try list.append(42);
    try std.testing.expectEqual(@as(i32, 43), list.pop());
}
