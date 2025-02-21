const fastnoise = @import("fastnoise.zig");
const std = @import("std");

// convenience
const print = std.debug.print;
fn print1(obj: anytype) void {
    print("{}", .{obj});
}
const expect = std.testing.expect;
const string = []const u8;

pub fn streql(a: string, b: string) bool {
    return std.mem.eql(u8, a, b);
}

const NodeType = enum { terrain, sig, filter };

const Param = struct {
    key: string,
    value: Value,
    pub fn deinit(self: Param, allocator: std.mem.Allocator) void {
        allocator.free(self.key);
    }
};
const ParamErrors = error{KeyNotFound};
fn getParam(params: []Param, key: string) !Value {
    for (params) |param| {
        if (std.mem.eql(u8, param.key, key)) {
            print("k:{s}, v: {}\n", .{ param.key, param.value });
            return param.value;
        }
    }
    return ParamErrors.KeyNotFound;
}

test "getParam" {
    const params = [_]Param{
        Param{ .key = "test", .value = Value{ .int = 123 } },
        Param{ .key = "test2", .value = Value{ .int = 12 } },
        Param{ .key = "test3", .value = Value{ .int = 1233 } },
    };

    try expect((try getParam(@constCast(&params), "test2")).int == 12);
    // try expect(getParam(@constCast(&params), "test4") == undefined);
}

const Value = union(enum) {
    float: f64,
    int: i64,
    // uint: u64,
};

const SignalChild = struct {
    offset: f64,
    child: WorldConf,

    pub fn deinit(self: SignalChild, allocator: std.mem.Allocator) void {
        deinitWorldconf(allocator, &self.child);
    }
};

const WorldConfSignal = union(enum) { perlin: PerlinSignal };
const PerlinSignal = struct {
    sig: fastnoise.Noise(f32),
    children: ?[]SignalChild,
    // id: u64,
    params: []Param,
    _type: NodeType,
    pub fn init(params: []Param, children: ?[]SignalChild) !PerlinSignal {
        const noise = try makePerlin(params);
        return PerlinSignal{ .params = params, .children = children, ._type = NodeType.sig, .sig = noise };
    }
    pub fn deinit(self: PerlinSignal, allocator: std.mem.Allocator) void {
        for (self.params) |c| c.deinit(allocator);
        allocator.free(self.params);
        if (self.children) |cc| {
            for (cc) |c| deinitWorldconf(allocator, &c.child);
            allocator.free(cc);
        }
    }
};

const WorldConfFilter = struct {
    name: string,
    params: []Param,
    _type: NodeType,
    source: *WorldConf,
    pub fn deinit(self: WorldConfFilter, allocator: std.mem.Allocator) void {
        for (self.params) |c| c.deinit(allocator);
        allocator.free(self.params);

        allocator.free(self.name);

        deinitWorldconf(allocator, self.source);

        allocator.destroy(self.source); // Free the pointer itself
    }
};

const WorldConfTerrain = struct {
    name: string,
    id: u64,
    color: u64,
    _type: NodeType,
    children: ?[]WorldConf,
    pub fn deinit(self: WorldConfTerrain, allocator: std.mem.Allocator) void {
        allocator.free(self.name);
        if (self.children) |cc| {
            for (cc) |c| {
                _ = deinitWorldconf(allocator, @constCast(&c));
            }
            allocator.free(cc);
        }
    }
};

const WorldConf = union(enum) { filter: WorldConfFilter, sig: WorldConfSignal, terrain: WorldConfTerrain };

fn parseParams(allocator: std.mem.Allocator, json: std.json.Array) ![]Param {
    var out = try allocator.alloc(Param, json.items.len);
    for (json.items, 0..) |p, idx| {
        const key = p.array.items[0].string;
        const value = p.array.items[1];
        out[idx] = Param{
            .key = try allocator.dupe(u8, key),
            .value = switch (value) {
                .float => |f| Value{ .float = f },
                .integer => |n| Value{ .int = @intCast(n) },
                else => unreachable,
            },
        };
    }
    return out;
}

const WorldConfError = error{ NodeNotObj, NoColorParamForTerrain, NoNameParam, MakeSignalError, UnsupportedSignalType };

fn makePerlin(params: []Param) !fastnoise.Noise(f32) {
    const freq: Value = try getParam(params, "freq");
    const seed: Value = try getParam(params, "seed");
    const octaves: Value = try getParam(params, "octaves");

    const noise = fastnoise.Noise(f32){
        .seed = @intCast(seed.int),
        .octaves = @intCast(octaves.int),
        .noise_type = .perlin,
        .frequency = @floatCast(freq.float),
        .gain = 0.80,
        .fractal_type = .fbm,
        .lacunarity = 2.0,
        .cellular_distance = .euclidean,
        .cellular_return = .distance2,
        .cellular_jitter_mod = 0.00,
    };
    return noise;
}

const SignalType = enum { perlin };

fn parseSignalJson(allocator: std.mem.Allocator, json: std.json.Value) !WorldConf {
    switch (json) {
        .object => {
            const params = try parseParams(allocator, json.object.get("params").?.array);
            const jchildren = json.object.get("children").?.array.items;
            var children = try allocator.alloc(SignalChild, jchildren.len);
            for (jchildren, 0..) |child, idx| {
                const offset = child.array.items[0].float;
                const tchild = try parseWorldConf(allocator, child.array.items[1]);
                children[idx] = SignalChild{ .offset = offset, .child = tchild };
            }
            const name = try allocator.dupe(u8, json.object.get("name").?.string);
            defer allocator.free(name); // just need name to resolve sig "type"
            const sigType = std.meta.stringToEnum(SignalType, name) orelse return WorldConfError.UnsupportedSignalType;
            const conf: WorldConf = switch (sigType) {
                .perlin => WorldConf{ .sig = WorldConfSignal{ .perlin = try PerlinSignal.init(params, children) } },
            };
            return conf;
        },
        else => {
            return WorldConfError.NodeNotObj;
        },
    }
}

fn parseTerrainJson(allocator: std.mem.Allocator, json: std.json.Value) !WorldConf {
    switch (json) {
        .object => {
            const name = if (json.object.get("name")) |n|
                try allocator.dupe(u8, n.string)
            else
                return WorldConfError.NoNameParam;
            const id: i64 = json.object.get("id").?.integer;
            const jchildren = json.object.get("children").?.array.items;
            var children = try allocator.alloc(WorldConf, jchildren.len);
            for (jchildren, 0..) |child, idx| {
                children[idx] = try parseWorldConf(allocator, child);
            }
            const color = json.object.get("color");
            if (color == null) return WorldConfError.NoColorParamForTerrain;
            const conf: WorldConf = WorldConf{ .terrain = WorldConfTerrain{ .children = children, .name = name, .color = @intCast(color.?.integer), .id = @intCast(id), ._type = NodeType.terrain } };
            return conf;
        },
        else => {
            return WorldConfError.NodeNotObj;
        },
    }
}

fn parseFilterJson(allocator: std.mem.Allocator, json: std.json.Value) !WorldConf {
    switch (json) {
        .object => {
            const name = try allocator.dupe(u8, json.object.get("name").?.string);
            const params = try parseParams(allocator, json.object.get("params").?.array);
            const source_ptr = try allocator.create(WorldConf);
            source_ptr.* = try parseWorldConf(allocator, json.object.get("source").?);
            const conf: WorldConf = WorldConf{ .filter = WorldConfFilter{ .params = params, .name = name, ._type = NodeType.filter, .source = source_ptr } };
            return conf;
        },
        else => {
            return WorldConfError.NodeNotObj;
        },
    }
}

pub fn parseWorldConf(allocator: std.mem.Allocator, json: std.json.Value) anyerror!WorldConf {
    switch (json) {
        .object => {
            const obj = json.object;

            const thisType = try allocator.dupe(u8, obj.get("type").?.string);
            defer allocator.free(thisType);

            if (streql(thisType, "signal")) {
                return try parseSignalJson(allocator, json);
            } else if (streql(thisType, "filter")) {
                return try parseFilterJson(allocator, json);
            } else {
                return try parseTerrainJson(allocator, json);
            }
        },
        else => {
            return WorldConfError.NodeNotObj;
        },
    }
}

pub fn deinitWorldconf(allocator: std.mem.Allocator, worldconf: *const WorldConf) void {
    switch (worldconf.*) {
        .sig => |sig| {
            switch (sig) {
                .perlin => sig.perlin.deinit(allocator),
            }
        },
        .terrain => {
            worldconf.*.terrain.deinit(allocator);
        },
        .filter => {
            worldconf.*.filter.deinit(allocator);
        },
    }
}

test {
    const alloc = std.testing.allocator;

    const file = try std.fs.cwd().openFile("./data2.json", .{});
    defer file.close();

    const json_string = try file.readToEndAlloc(alloc, 1024 * 1024);
    defer alloc.free(json_string);

    // const conf = try std.json.parseFromSlice(WorldConfTerrain, alloc, json_string, .{});
    // std.debug.print("{s}", .{conf.value.name});
    var tree = try std.json.parseFromSlice(std.json.Value, alloc, json_string, .{});
    defer tree.deinit();

    var world_conf = try parseWorldConf(alloc, tree.value);
    defer deinitWorldconf(alloc, &world_conf);
    try std.testing.expect(world_conf.terrain.name.len > 0);
    // std.debug.print("The parent terrain: {s}\n", .{world_conf.name});
}

test "fastnoise" {
    const noise = fastnoise.Noise(f32){
        .seed = 133121,
        .noise_type = .perlin,
        .frequency = 0.40025,
        .gain = 0.40,
        .fractal_type = .fbm,
        .lacunarity = 0.40,
        .cellular_distance = .euclidean,
        .cellular_return = .distance2,
        .cellular_jitter_mod = 0.00,
    };

    const size = 128;
    var values: [size * size]f32 = undefined;
    for (0..values.len) |i| values[i] = noise.genNoise2D(@floatFromInt(500 + (i % size)), @floatFromInt(i / size));
    // try expect(values[3] > 0);
    print("{d}\n", .{values[3]});
    print("{d}\n", .{values[2]});
    print("{d}\n", .{values[100]});
    print("{d}\n", .{values[30]});

    // for (0..values.len) |i| print("{d}\n", .{values[i]});
}
