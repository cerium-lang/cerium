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
        .f32, .f64 => true,

        else => false,
    };
}

pub fn canBeNegative(self: Type) bool {
    return switch (self.tag) {
        .ambigiuous_int, .ambigiuous_float, .i8, .i16, .i32, .i64, .f32, .f64 => true,

        else => false,
    };
}
