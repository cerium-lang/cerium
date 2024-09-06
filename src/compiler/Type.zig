const std = @import("std");

const Type = @This();

tag: Tag,
data: Data = .none,

pub const Tag = enum {
    void,
    pointer,
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

pub const Data = union(enum) {
    none,
    pointer: Pointer,

    pub const Pointer = struct {
        size: Size,
        is_const: bool,
        child: *const Type,

        pub const Size = enum {
            one,
            many,
        };
    };
};

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

pub fn format(self: Type, _: anytype, _: anytype, writer: anytype) !void {
    switch (self.tag) {
        .void => try writer.writeAll("void"),

        .pointer => {
            if (self.data.pointer.size == .one) {
                try writer.writeAll("*");
            } else if (self.data.pointer.size == .many) {
                try writer.writeAll("[*]");
            }

            if (self.data.pointer.is_const) {
                try writer.writeAll("const ");
            }

            try writer.print("{}", .{self.data.pointer.child});
        },

        .ambigiuous_int => try writer.writeAll("ambigiuous_int"),
        .ambigiuous_float => try writer.writeAll("ambigiuous_float"),
        .u8 => try writer.writeAll("u8"),
        .u16 => try writer.writeAll("u16"),
        .u32 => try writer.writeAll("u32"),
        .u64 => try writer.writeAll("u64"),
        .i8 => try writer.writeAll("i8"),
        .i16 => try writer.writeAll("i16"),
        .i32 => try writer.writeAll("i32"),
        .i64 => try writer.writeAll("i64"),
        .f32 => try writer.writeAll("f32"),
        .f64 => try writer.writeAll("f64"),
    }
}

pub fn eql(self: Type, other: Type) bool {
    if (self.tag != other.tag) {
        return false;
    }

    if (self.tag == .pointer and
        (!self.data.pointer.child.eql(other.data.pointer.child.*) or
        (self.data.pointer.is_const and !other.data.pointer.is_const) or
        self.data.pointer.size != other.data.pointer.size))
    {
        return false;
    }

    return true;
}
