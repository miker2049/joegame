const std = @import("std");

// const WorldConfSignal = struct { children: []struct { weight: f64, next: WorldConf } };

const WorldConf = struct {
    children: []Child,
    color: ?u32 = null,
    name: []const u8,
    type: []const u8,
    id: ?u32 = null,
    params: ?[]Param = null,
    source: ?*WorldConf = null,
    pub fn deinit(conf: WorldConf, allocator: std.mem.Allocator) void {
        allocator.free(conf.name);
        allocator.free(conf.type);

        for (conf.children) |child| {
            switch (child) {
                .sig => |sig| sig.conf.deinit(allocator),
                .terr => |terr| terr.deinit(allocator),
            }
        }
        if (conf.children.len > 0) {
            allocator.free(conf.children);
        }

        if (conf.params) |params| {
            for (params) |param| {
                allocator.free(param.key);
            }
            allocator.free(params);
        }

        if (conf.source) |source| {
            source.*.deinit(allocator);
            allocator.destroy(source);
        }
    }
};

const SignalChild = struct {
    weight: f64,
    conf: WorldConf,
};
const TerrChild = WorldConf;
const Child = union(enum) { sig: SignalChild, terr: TerrChild };

const Param = struct {
    key: []const u8,
    value: Value,
};

const Value = union(enum) {
    float: f64,
    int: i64,
    uint: u64,
};

pub fn parseWorldConf(allocator: std.mem.Allocator, json: std.json.Value) !WorldConf {
    const obj = json.object;

    var conf = WorldConf{
        .children = &[_]Child{},
        .name = try allocator.dupe(u8, obj.get("name").?.string),
        .type = try allocator.dupe(u8, obj.get("type").?.string),
        .color = if (obj.get("color")) |c| @intCast(c.integer) else null,
        .id = if (obj.get("id")) |i| @intCast(i.integer) else null,
    };

    if (obj.get("children")) |children| {
        if (std.mem.eql(u8, conf.type, "terrain")) {
            const child_array = children.array;
            conf.children = try allocator.alloc(Child, child_array.items.len);
            for (child_array.items, 0..) |child, i| {
                switch (child) {
                    .array => {
                        conf.children[i] = Child{ .sig = SignalChild{ .weight = child.array.items[0].float, .conf = try parseWorldConf(allocator, child.array.items[1]) } };
                    },
                    .object => {
                        conf.children[i] = Child{ .terr = try parseWorldConf(allocator, child) };
                    },
                    else => {},
                }
            }
        }
    }

    if (obj.get("params")) |params| {
        const param_array = params.array;
        conf.params = try allocator.alloc(Param, param_array.items.len);
        for (param_array.items, 0..) |param, i| {
            const key = param.array.items[0].string;
            const value = param.array.items[1];
            conf.params.?[i] = Param{
                .key = try allocator.dupe(u8, key),
                .value = switch (value) {
                    .float => |f| Value{ .float = f },
                    .integer => |n| Value{ .int = @intCast(n) },
                    else => unreachable,
                },
            };
        }
    }

    if (obj.get("source")) |src| {
        const source = try allocator.create(WorldConf);
        source.* = try parseWorldConf(allocator, src);
        conf.source = source;
    }

    return conf;
}

test {
    const alloc = std.testing.allocator;

    const file = try std.fs.cwd().openFile("./test.json", .{});
    defer file.close();

    const json_string = try file.readToEndAlloc(alloc, 1024 * 1024);
    defer alloc.free(json_string);

    var tree = try std.json.parseFromSlice(std.json.Value, alloc, json_string, .{});
    defer tree.deinit();

    const world_conf = try parseWorldConf(alloc, tree.value);
    defer world_conf.deinit(alloc);

    std.debug.print("The parent terrain: {s}\n", .{world_conf.name});
}
