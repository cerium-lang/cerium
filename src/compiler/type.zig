pub const Type = enum {
    void_type,
    string_type,
    char_type,
    int_type,
    float_type,

    pub fn to_string(self: Type) []const u8 {
        return switch (self) {
            .void_type => "void",
            .string_type => "string",
            .char_type => "char",
            .int_type => "int",
            .float_type => "float",
        };
    }
};
