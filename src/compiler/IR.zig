const std = @import("std");

const Aarch64Renderer = @import("renderers/Aarch64Renderer.zig");

const IR = @This();

instructions: []const Instruction,
string_literals: [][]const u8,

pub const Value = union(enum) {
    variable_reference: VariableReference,
    string_reference: StringReference,
    char: Char,
    int: Int,
    float: Float,

    pub const VariableReference = struct { name: []const u8 };

    pub const StringReference = struct { index: usize };

    pub const Char = struct {
        value: u8,
    };

    pub const Int = struct {
        value: i64,
    };

    pub const Float = struct {
        value: f64,
    };
};

pub const Instruction = union(enum) {
    store: Store,
    load: Load,
    label: Label,
    ret: Ret,

    pub const Store = struct {
        name: []const u8,
        value: Value,
    };

    pub const Load = struct { value: Value };

    pub const Label = struct { name: []const u8 };

    pub const Ret = struct {};
};

pub const Error = error{UnsupportedTarget} || std.mem.Allocator.Error;

pub fn render(self: IR, gpa: std.mem.Allocator, target: std.Target) Error![]const u8 {
    switch (target.cpu.arch) {
        .aarch64 => {
            var renderer = Aarch64Renderer.init(gpa, self);

            try renderer.render();

            return try renderer.dump();
        },

        else => return error.UnsupportedTarget,
    }
}
