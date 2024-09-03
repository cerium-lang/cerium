const std = @import("std");

const IR = @This();

instructions: []const Instruction,
string_literals: [][]const u8,

pub const Instruction = union(enum) {
    store: Store,
    load: Load,
    label: Label,
    function_proluge,
    function_epilogue,
    inline_assembly: InlineAssembly,
    @"return",

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
