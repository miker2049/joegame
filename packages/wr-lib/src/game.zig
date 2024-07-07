const std = @import("std");
const Position = struct { x: usize, y: usize };
const Rect = struct { x: usize, y: usize, width: usize, height: usize };
const GameView = Rect;

pub fn make_view(x: usize, y: usize, width: usize, height: usize) GameView {
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

fn get_min(arr: []const usize) usize {
    var min: usize = @bitCast(std.math.inf(f64));
    for (arr) |val| {
        if (val < min) min = val;
    }
    return min;
}

fn normalize_elev(arr: *[]const usize) void {
    const min = get_min(arr);
    _ = min;
    for (arr.*) |*val| {
        _ = val;
        // std.debug.print("{}, {}\n", .{ val.*, min });
    }
}

const Message = struct { index: usize, message: []u8 };
const Monster = struct { pos: usize, power: usize, thoughts: []u8, name: []u8, description: []u8 };
pub const GameConfig = struct { elevations: []usize, messages: []Message, view: GameView, monsters: []Monster };

pub const GameState = struct {
    view: GameView,
    position: Position,
    elevations: []const usize,
    buffer: []u21,
    min: usize,
    allocator: *const std.mem.Allocator,
    messages: []Message,
    pub fn init(conf: GameConfig, allocator: *const std.mem.Allocator) !GameState {
        const view = make_view(conf.view.x, conf.view.y, conf.view.width, conf.view.height);
        const newBuff = try allocator.alloc(u21, @intCast(conf.view.width * conf.view.height));
        const min = get_min(conf.elevations);
        // const pos = Position{ .x = 0, .y = 0 };
        var state = GameState{ .position = Position{ .x = 0, .y = 0 }, .allocator = allocator, .min = min, .buffer = newBuff, .view = view, .elevations = conf.elevations, .messages = conf.messages };

        try state.render();
        return state;
    }
    pub fn free(self: *GameState) void {
        self.allocator.free(self.buffer);
    }
    pub fn move_view(self: *GameState, x: usize, y: usize) void {
        self.view.x = @max(0, x);
        self.view.y = @max(0, y);
        try self.render();
    }
    pub fn move_pos(self: *GameState, x: usize, y: usize) void {
        self.position.x = @max(0, x);
        self.position.y = @max(0, y);
        try self.render();
    }
    pub fn dec_pos_y(self: *GameState, n: usize) void {
        self.move_pos(self.position.x, self.position.y - n);
    }
    pub fn inc_pos_y(self: *GameState, n: usize) void {
        self.move_pos(self.position.x, self.position.y + n);
    }
    pub fn dec_pos_x(self: *GameState, n: usize) void {
        self.move_pos(self.position.x - n, self.position.y);
    }
    pub fn inc_pos_x(self: *GameState, n: usize) void {
        self.move_pos(self.position.x + n, self.position.y);
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
    pub fn get_elev(self: *GameState, x: usize) usize {
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
                    self.buffer[idx] = 0x0022;
                }
                // std.debug.print("{} < {}\n", .{ row, (self.get_elev(cell) - self.min) });
            }
        }

        //self.writer.print("hey yo");
    }
};
