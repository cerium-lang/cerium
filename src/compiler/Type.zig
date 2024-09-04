const Type = @This();

tag: Tag,

pub const Tag = enum {
    void_type,
    string_type,
    u8_type,
    u16_type,
    u32_type,
    u64_type,
    i8_type,
    i16_type,
    i32_type,
    i64_type,
    f32_type,
    f64_type,
};

pub fn toString(self: Type) []const u8 {
    return switch (self.tag) {
        .void_type => "void",
        .string_type => "string",
        .u8_type => "u8",
        .u16_type => "u16",
        .u32_type => "u32",
        .u64_type => "u64",
        .i8_type => "i8",
        .i16_type => "i16",
        .i32_type => "i32",
        .i64_type => "i64",
        .f32_type => "f32",
        .f64_type => "f64",
    };
}

pub fn isInt(self: Type) bool {
    return switch (self.tag) {
        .u8_type, .u16_type, .u32_type, .u64_type, .i8_type, .i16_type, .i32_type, .i64_type => true,

        else => false,
    };
}

pub fn isFloat(self: Type) bool {
    return switch (self.tag) {
        .f32_type, .f64_type => true,

        else => false,
    };
}
