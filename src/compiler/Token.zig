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
    equal_sign,
    double_equal_sign,
    plus,
    minus,
    star,
    forward_slash,
    comma,
    semicolon,
    keyword_fn,
    keyword_let,
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
    .{ "let", .keyword_let },
    .{ "const", .keyword_const },
    .{ "asm", .keyword_asm },
    .{ "return", .keyword_return },
});
