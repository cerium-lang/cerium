const std = @import("std");

const Aarch64Renderer = @import("renderers/Aarch64Renderer.zig");
const Amd64Renderer = @import("renderers/Amd64Renderer.zig");

const IR = @This();

instructions: []const Instruction,
string_literals: [][]const u8,

pub const Instruction = union(enum) {
    store: Store,
    load: Load,
    label: Label,
    inline_assembly: InlineAssembly,
    @"return": void,

    pub const Store = struct {
        name: []const u8,
    };

    pub const Load = union(enum) {
        name: []const u8,
        string: usize,
        char: u8,
        int: i64,
        float: f64,
    };

    pub const Label = struct {
        name: []const u8,
    };

    pub const InlineAssembly = struct {
        content: []const u8,
    };
};

pub const Error = error{UnsupportedTarget} || Aarch64Renderer.Error || Amd64Renderer.Error;

pub fn render(self: IR, gpa: std.mem.Allocator, target: std.Target) Error![]const u8 {
    return switch (target.cpu.arch) {
        .aarch64 => blk: {
            var renderer = Aarch64Renderer.init(gpa, self);

            try renderer.render();

            break :blk renderer.dump();
        },

        .x86_64 => blk: {
            var renderer = Amd64Renderer.init(gpa, self);

            try renderer.render();

            break :blk renderer.dump();
        },

        else => error.UnsupportedTarget,
    };
}
