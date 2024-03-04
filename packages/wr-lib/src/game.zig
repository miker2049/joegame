const std = @import("std");
const Rect = struct { x: i64, y: i64, width: i64, height: i64 };
const GameView = Rect;

pub fn make_view(x: i64, y: i64, width: i64, height: i64) GameView {
    return GameView{ .x = x, .y = y, .width = width, .height = height };
}

// pub fn alloc2d(allocator: *const std.mem.Allocator, m: usize, n: usize) ![][]u8 {
//     var array = try allocator.alloc([]u8, m);
//     for (array) |*row| {
//         row.* = try allocator.alloc(u8, n);
//     }
//     return array;
// }

// fn free2d(allocator: *std.mem.Allocator, array: [][]i64) void {
//     for (array) |row| {
//         allocator.free(row);
//     }
//     allocator.free(array);
// }

fn get_min(arr: []const i16) i16 {
    var min = @as(i16, @bitCast(std.math.inf(f16)));
    for (arr) |val| {
        if (val < min) min = val;
    }
    return min;
}

fn normalize_elev(arr: *[]const i16) void {
    const min = get_min(arr);
    _ = min;
    for (arr.*) |*val| {
        _ = val;
        // std.debug.print("{}, {}\n", .{ val.*, min });
    }
}

pub const GameState = struct {
    view: GameView,
    elevations: []const i16,
    buffer: []u21,
    min: i16,
    allocator: *const std.mem.Allocator,
    pub fn init(x: i64, y: i64, comptime width: i64, comptime height: i64, elevations: []const i16, allocator: *const std.mem.Allocator) !GameState {
        var view = make_view(x, y, width, height);
        const newBuff = try allocator.alloc(u21, @intCast(width * height));
        const min = get_min(elevations);
        var state = GameState{ .allocator = allocator, .min = min, .buffer = newBuff, .view = view, .elevations = elevations };
        try state.render();
        return state;
    }
    pub fn free(self: *GameState) void {
        self.allocator.free(self.buffer);
    }
    pub fn move_view(self: *GameState, x: i64, y: i64) void {
        self.view.x = @max(0, x);
        self.view.y = @max(0, y);
        try self.render();
    }
    pub fn get_index(self: *GameState, x: usize, y: usize) usize {
        const w: usize = @intCast(self.view.width);
        return (w * y) + x;
    }
    pub fn get_row(self: *GameState, y: usize) @TypeOf(self.buffer) {
        const w: usize = @intCast(self.view.width);
        const start = y * w;
        const end = start + w;
        return self.buffer[start..end];
    }
    pub fn get_elev(self: *GameState, x: usize) i16 {
        return self.elevations[x];
    }
    pub fn render(self: *GameState) !void {
        const x: usize = @intCast(self.view.x);
        const y: usize = @intCast(self.view.y);
        for (y..@intCast(self.view.y + self.view.height)) |row| {
            for (x..@intCast(self.view.x + self.view.width)) |cell| {
                const idx = self.get_index(cell - x, row - y);
                if (row < (self.get_elev(cell) - self.min)) {
                    self.buffer[idx] = 0x0023;
                } else {
                    self.buffer[idx] = " "[0];
                }
                // std.debug.print("{} < {}\n", .{ row, (self.get_elev(cell) - self.min) });
            }
        }
        //self.writer.print("hey yo");
    }
};
