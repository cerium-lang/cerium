const std = @import("std");

tag: Tag,
buffer_loc: BufferLoc,

pub const Tag = enum { eof, invalid, identifier, string_literal, char_literal, int, float, open_paren, close_paren, open_brace, close_brace, equal_sign, double_equal_sign, comma, keyword_fn, keyword_let, keyword_return };

pub const BufferLoc = struct {
    start: usize,
    end: usize,
};

pub const Keywords = std.ComptimeStringMap(Tag, .{ .{ "fn", .keyword_fn }, .{ "let", .keyword_let }, .{ "return", .keyword_return } });
