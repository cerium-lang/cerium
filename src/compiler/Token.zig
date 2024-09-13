const std = @import("std");

tag: Tag,
buffer_loc: BufferLoc,

pub const Tag = enum {
    eof,
    invalid,
    identifier,
    string_literal,
    char_literal,
    int,
    float,
    open_paren,
    close_paren,
    open_brace,
    close_brace,
    open_bracket,
    close_bracket,
    bang,
    tilde,
    ampersand,
    comma,
    colon,
    semicolon,
    plus,
    minus,
    star,
    less_than,
    double_less_than,
    greater_than,
    double_greater_than,
    equal_sign,
    double_equal_sign,
    bang_equal_sign,
    forward_slash,
    keyword_fn,
    keyword_var,
    keyword_const,
    keyword_asm,
    keyword_return,
};

pub const BufferLoc = struct {
    start: usize,
    end: usize,
};

pub const keywords = std.StaticStringMap(Tag).initComptime(.{
    .{ "fn", .keyword_fn },
    .{ "var", .keyword_var },
    .{ "const", .keyword_const },
    .{ "asm", .keyword_asm },
    .{ "return", .keyword_return },
});
