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
    greater_than,
    equal_sign,
    double_equal_sign,
    bang_equal_sign,
    forward_slash,
    keyword_fn,
    keyword_let,
    keyword_const,
    keyword_asm,
    keyword_return,
    keyword_true,
    keyword_false,
};

pub const BufferLoc = struct {
    start: usize,
    end: usize,
};

pub const keywords = std.StaticStringMap(Tag).initComptime(.{
    .{ "fn", .keyword_fn },
    .{ "let", .keyword_let },
    .{ "const", .keyword_const },
    .{ "asm", .keyword_asm },
    .{ "return", .keyword_return },
    .{ "true", .keyword_true },
    .{ "false", .keyword_false },
});
