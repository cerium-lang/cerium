const std = @import("std");

const Type = @This();

tag: Tag,

pub const Tag = enum {
    void,
    string,
    ambigiuous_int,
    ambigiuous_float,
    u8,
    u16,
    u32,
    u64,
    i8,
    i16,
    i32,
    i64,
    f32,
    f64,
};

pub fn toString(self: Type) []const u8 {
    return switch (self.tag) {
        .void => "void",
        .string => "string",
        .ambigiuous_int => "ambigiuous_int",
        .ambigiuous_float => "ambigiuous_float",
        .u8 => "u8",
        .u16 => "u16",
        .u32 => "u32",
        .u64 => "u64",
        .i8 => "i8",
        .i16 => "i16",
        .i32 => "i32",
        .i64 => "i64",
        .f32 => "f32",
        .f64 => "f64",
    };
}

pub fn isInt(self: Type) bool {
    return switch (self.tag) {
        .ambigiuous_int, .u8, .u16, .u32, .u64, .i8, .i16, .i32, .i64 => true,

        else => false,
    };
}

pub fn isFloat(self: Type) bool {
    return switch (self.tag) {
        .ambigiuous_float, .f32, .f64 => true,

        else => false,
    };
}

pub fn isIntOrFloat(self: Type) bool {
    return (self.isInt() or self.isFloat());
}

pub fn minInt(self: Type) i128 {
    return switch (self.tag) {
        .ambigiuous_int => std.math.minInt(i128),
        .u8 => std.math.minInt(u8),
        .u16 => std.math.minInt(u16),
        .u32 => std.math.minInt(u32),
        .u64 => std.math.minInt(u64),
        .i8 => std.math.minInt(i8),
        .i16 => std.math.minInt(i16),
        .i32 => std.math.minInt(i32),
        .i64 => std.math.minInt(i64),

        else => unreachable,
    };
}

pub fn maxInt(self: Type) i128 {
    return switch (self.tag) {
        .ambigiuous_int => std.math.maxInt(i128),
        .u8 => std.math.maxInt(u8),
        .u16 => std.math.maxInt(u16),
        .u32 => std.math.maxInt(u32),
        .u64 => std.math.maxInt(u64),
        .i8 => std.math.maxInt(i8),
        .i16 => std.math.maxInt(i16),
        .i32 => std.math.maxInt(i32),
        .i64 => std.math.maxInt(i64),

        else => unreachable,
    };
}

pub fn minFloat(self: Type) f64 {
    return switch (self.tag) {
        .f32 => std.math.floatMin(f32),
        .ambigiuous_float, .f64 => std.math.floatMin(f64),

        else => unreachable,
    };
}

pub fn maxFloat(self: Type) f64 {
    return switch (self.tag) {
        .f32 => std.math.floatMax(f32),
        .ambigiuous_float, .f64 => std.math.floatMax(f64),

        else => unreachable,
    };
}

pub fn canBeNegative(self: Type) bool {
    return switch (self.tag) {
        .ambigiuous_int, .ambigiuous_float, .i8, .i16, .i32, .i64, .f32, .f64 => true,

        else => false,
    };
}

pub fn isAmbigiuous(self: Type) bool {
    return switch (self.tag) {
        .ambigiuous_int, .ambigiuous_float => true,

        else => false,
    };
}
