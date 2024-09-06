const std = @import("std");

const Token = @import("Token.zig");

const Lexer = @This();

buffer: [:0]const u8,
index: usize,
state: State,

pub const State = enum {
    start,
    identifier,
    string_literal,
    char_literal,
    number,
    equal_sign,
};

pub fn init(buffer: [:0]const u8) Lexer {
    return Lexer{
        .buffer = buffer,
        .index = 0,
        .state = .start,
    };
}

pub fn next(self: *Lexer) Token {
    var result = Token{
        .tag = .eof,
        .buffer_loc = .{
            .start = self.index,
            .end = self.index,
        },
    };

    while (self.buffer.len >= self.index) : (self.index += 1) {
        const current_char = self.buffer[self.index];

        switch (self.state) {
            .start => switch (current_char) {
                0 => break,

                ' ', '\r', '\n', '\t' => {},

                'a'...'z', 'A'...'Z', '_' => {
                    result.buffer_loc.start = self.index;
                    result.tag = .identifier;
                    self.state = .identifier;
                },

                '"' => {
                    result.buffer_loc.start = self.index + 1;
                    result.tag = .string_literal;
                    self.state = .string_literal;
                },

                '\'' => {
                    result.buffer_loc.start = self.index + 1;
                    result.tag = .char_literal;
                    self.state = .char_literal;
                },

                '0'...'9' => {
                    result.buffer_loc.start = self.index;
                    result.tag = .int;
                    self.state = .number;
                },

                '(' => {
                    result.buffer_loc.start = self.index;
                    self.index += 1;
                    result.buffer_loc.end = self.index;
                    result.tag = .open_paren;
                    break;
                },

                ')' => {
                    result.buffer_loc.start = self.index;
                    self.index += 1;
                    result.buffer_loc.end = self.index;
                    result.tag = .close_paren;
                    break;
                },

                '{' => {
                    result.buffer_loc.start = self.index;
                    self.index += 1;
                    result.buffer_loc.end = self.index;
                    result.tag = .open_brace;
                    break;
                },

                '}' => {
                    result.buffer_loc.start = self.index;
                    self.index += 1;
                    result.buffer_loc.end = self.index;
                    result.tag = .close_brace;
                    break;
                },

                '=' => {
                    result.buffer_loc.start = self.index;
                    result.tag = .equal_sign;
                    self.state = .equal_sign;
                },

                '+' => {
                    result.buffer_loc.start = self.index;
                    self.index += 1;
                    result.buffer_loc.end = self.index;
                    result.tag = .plus;
                    break;
                },

                '-' => {
                    result.buffer_loc.start = self.index;
                    self.index += 1;
                    result.buffer_loc.end = self.index;
                    result.tag = .minus;
                    break;
                },

                '*' => {
                    result.buffer_loc.start = self.index;
                    self.index += 1;
                    result.buffer_loc.end = self.index;
                    result.tag = .star;
                    break;
                },

                '/' => {
                    result.buffer_loc.start = self.index;
                    self.index += 1;
                    result.buffer_loc.end = self.index;
                    result.tag = .forward_slash;
                    break;
                },

                ',' => {
                    result.buffer_loc.start = self.index;
                    self.index += 1;
                    result.buffer_loc.end = self.index;
                    result.tag = .comma;
                    break;
                },

                ';' => {
                    result.buffer_loc.start = self.index;
                    self.index += 1;
                    result.buffer_loc.end = self.index;
                    result.tag = .semicolon;
                    break;
                },

                else => {
                    result.buffer_loc.start = self.index;
                    self.index += 1;
                    result.buffer_loc.end = self.index;
                    result.tag = .invalid;
                    break;
                },
            },

            .identifier => switch (current_char) {
                'a'...'z', 'A'...'Z', '0'...'9', '_' => {},

                else => {
                    result.buffer_loc.end = self.index;
                    if (Token.keywords.get(self.buffer[result.buffer_loc.start..result.buffer_loc.end])) |keyword_tag| {
                        result.tag = keyword_tag;
                    }
                    self.state = .start;
                    break;
                },
            },

            .string_literal => switch (current_char) {
                0 => {
                    result.buffer_loc.end = self.index;
                    self.state = .start;
                    result.tag = .invalid;
                    break;
                },

                '\n' => {
                    result.buffer_loc.end = self.index;
                    self.index += 1;
                    self.state = .start;
                    result.tag = .invalid;
                    break;
                },

                '"' => {
                    result.buffer_loc.end = self.index;
                    self.index += 1;
                    self.state = .start;
                    break;
                },

                else => {},
            },

            .char_literal => switch (current_char) {
                0 => {
                    result.buffer_loc.end = self.index;
                    self.state = .start;
                    result.tag = .invalid;
                    break;
                },

                '\n' => {
                    result.buffer_loc.end = self.index;
                    self.index += 1;
                    self.state = .start;
                    result.tag = .invalid;
                    break;
                },

                '\'' => {
                    result.buffer_loc.end = self.index;
                    self.index += 1;
                    self.state = .start;
                    break;
                },

                else => {},
            },

            .number => switch (current_char) {
                'a'...'z', 'A'...'Z', '0'...'9', '_' => {},

                '.' => {
                    result.tag = .float;
                },

                else => {
                    result.buffer_loc.end = self.index;
                    self.state = .start;
                    break;
                },
            },

            .equal_sign => switch (current_char) {
                '=' => {
                    self.index += 1;
                    result.buffer_loc.end = self.index;
                    result.tag = .double_equal_sign;
                    self.state = .start;
                    break;
                },

                else => {
                    result.buffer_loc.end = self.index;
                    self.state = .start;
                    break;
                },
            },
        }
    }

    return result;
}
