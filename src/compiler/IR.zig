const std = @import("std");

instructions: []const Instruction,
string_literals: [][]const u8,

const Value = union(enum) {
    string_reference: StringReference,
    char: Char,
    int: Int,
    float: Float,

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

const Instruction = union(enum) {
    label: Label,
    ret: Ret,

    const Label = struct { name: []const u8 };

    const Ret = struct {
        value: Value,
    };
};
