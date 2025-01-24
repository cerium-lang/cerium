const std = @import("std");

const Token = @import("Token.zig");

const Lexer = @This();

buffer: [:0]const u8,
index: u32,

pub const State = enum {
    start,
    identifier,
    string_literal,
    string_literal_back_slash,
    char_literal,
    char_literal_back_slash,
    number,
    number_saw_period,
    divide,
    comment,
    assign,
    bool_not,
    period,
    double_period,
    less_than,
    greater_than,
};

pub fn init(buffer: [:0]const u8) Lexer {
    return Lexer{
        .buffer = buffer,
        .index = 0,
    };
}

pub fn next(self: *Lexer) Token {
    var result = Token{
        .tag = .eof,
        .range = .{
            .start = self.index,
            .end = self.index,
        },
    };

    state: switch (State.start) {
        .start => switch (self.buffer[self.index]) {
            0 => {
                result = .{
                    .tag = .eof,
                    .range = .{
                        .start = self.index,
                        .end = self.index,
                    },
                };
            },

            ' ', '\r', '\n', '\t' => {
                self.index += 1;
                continue :state .start;
            },

            'a'...'z', 'A'...'Z', '_' => {
                result.range.start = self.index;
                result.tag = .identifier;
                self.index += 1;
                continue :state .identifier;
            },

            '"' => {
                result.range.start = self.index + 1;
                result.tag = .string_literal;
                self.index += 1;
                continue :state .string_literal;
            },

            '\'' => {
                result.range.start = self.index + 1;
                result.tag = .char_literal;
                self.index += 1;
                continue :state .char_literal;
            },

            '0'...'9' => {
                result.range.start = self.index;
                result.tag = .int;
                self.index += 1;
                continue :state .number;
            },

            '(' => {
                result.range.start = self.index;
                self.index += 1;
                result.range.end = self.index;
                result.tag = .open_paren;
            },

            ')' => {
                result.range.start = self.index;
                self.index += 1;
                result.range.end = self.index;
                result.tag = .close_paren;
            },

            '{' => {
                result.range.start = self.index;
                self.index += 1;
                result.range.end = self.index;
                result.tag = .open_brace;
            },

            '}' => {
                result.range.start = self.index;
                self.index += 1;
                result.range.end = self.index;
                result.tag = .close_brace;
            },

            '[' => {
                result.range.start = self.index;
                self.index += 1;
                result.range.end = self.index;
                result.tag = .open_bracket;
            },

            ']' => {
                result.range.start = self.index;
                self.index += 1;
                result.range.end = self.index;
                result.tag = .close_bracket;
            },

            '=' => {
                result.range.start = self.index;
                result.tag = .assign;
                self.index += 1;
                continue :state .assign;
            },

            '!' => {
                result.range.start = self.index;
                result.tag = .bool_not;
                self.index += 1;
                continue :state .bool_not;
            },

            '<' => {
                result.range.start = self.index;
                result.tag = .less_than;
                self.index += 1;
                continue :state .less_than;
            },

            '>' => {
                result.range.start = self.index;
                result.tag = .greater_than;
                self.index += 1;
                continue :state .greater_than;
            },

            '~' => {
                result.range.start = self.index;
                self.index += 1;
                result.range.end = self.index;
                result.tag = .bit_not;
            },

            '.' => {
                result.range.start = self.index;
                result.tag = .period;
                self.index += 1;
                continue :state .period;
            },

            '%' => {
                result.range.start = self.index;
                self.index += 1;
                result.range.end = self.index;
                result.tag = .modulo;
            },

            '+' => {
                result.range.start = self.index;
                self.index += 1;
                result.range.end = self.index;
                result.tag = .plus;
            },

            '-' => {
                result.range.start = self.index;
                self.index += 1;
                result.range.end = self.index;
                result.tag = .minus;
            },

            '*' => {
                result.range.start = self.index;
                self.index += 1;
                result.range.end = self.index;
                result.tag = .star;
            },

            '/' => {
                result.range.start = self.index;
                self.index += 1;
                result.tag = .divide;
                continue :state .divide;
            },

            '&' => {
                result.range.start = self.index;
                self.index += 1;
                result.range.end = self.index;
                result.tag = .bit_and;
            },

            '|' => {
                result.range.start = self.index;
                self.index += 1;
                result.range.end = self.index;
                result.tag = .bit_or;
            },

            '^' => {
                result.range.start = self.index;
                self.index += 1;
                result.range.end = self.index;
                result.tag = .bit_xor;
            },

            ',' => {
                result.range.start = self.index;
                self.index += 1;
                result.range.end = self.index;
                result.tag = .comma;
            },

            ':' => {
                result.range.start = self.index;
                self.index += 1;
                result.range.end = self.index;
                result.tag = .colon;
            },

            ';' => {
                result.range.start = self.index;
                self.index += 1;
                result.range.end = self.index;
                result.tag = .semicolon;
            },

            else => {
                result.range.start = self.index;
                self.index += 1;
                result.range.end = self.index;
                result.tag = .invalid;
            },
        },

        .identifier => switch (self.buffer[self.index]) {
            'a'...'z', 'A'...'Z', '0'...'9', '_', ':' => {
                self.index += 1;
                continue :state .identifier;
            },

            else => {
                result.range.end = self.index;

                if (Token.keywords.get(self.buffer[result.range.start..result.range.end])) |keyword_tag| {
                    result.tag = keyword_tag;
                }
            },
        },

        .string_literal => switch (self.buffer[self.index]) {
            0, '\n' => {
                result.range.end = self.index;
                result.tag = .invalid;
            },

            '"' => {
                result.range.end = self.index;
                self.index += 1;
            },

            '\\' => {
                self.index += 1;
                continue :state .string_literal_back_slash;
            },

            else => {
                self.index += 1;
                continue :state .string_literal;
            },
        },

        .string_literal_back_slash => switch (self.buffer[self.index]) {
            0, '\n' => {
                result.range.end = self.index;
                result.tag = .invalid;
            },

            else => {
                self.index += 1;
                continue :state .string_literal;
            },
        },

        .char_literal => switch (self.buffer[self.index]) {
            0, '\n' => {
                result.range.end = self.index;
                result.tag = .invalid;
            },

            '\'' => {
                result.range.end = self.index;
                self.index += 1;
            },

            '\\' => {
                self.index += 1;
                continue :state .char_literal_back_slash;
            },

            else => {
                self.index += 1;
                continue :state .char_literal;
            },
        },

        .char_literal_back_slash => switch (self.buffer[self.index]) {
            0, '\n' => {
                result.range.end = self.index;
                result.tag = .invalid;
            },

            else => {
                self.index += 1;
                continue :state .char_literal;
            },
        },

        .number => switch (self.buffer[self.index]) {
            'a'...'z', 'A'...'Z', '0'...'9', '_' => {
                self.index += 1;
                continue :state .number;
            },

            '.' => {
                result.tag = .float;
                self.index += 1;
                continue :state .number_saw_period;
            },

            else => {
                result.range.end = self.index;
            },
        },

        .number_saw_period => switch (self.buffer[self.index]) {
            '.' => {
                result.tag = .int;
                self.index -= 1;
                result.range.end = self.index;
            },

            else => continue :state .number,
        },

        .divide => switch (self.buffer[self.index]) {
            '/' => {
                self.index += 1;
                continue :state .comment;
            },

            else => {
                result.range.end = self.index;
            },
        },

        .comment => switch (self.buffer[self.index]) {
            0 => {
                result.tag = .eof;
                result.range.start = self.index;
                result.range.end = self.index;
            },

            '\n' => {
                self.index += 1;
                continue :state .start;
            },

            else => {
                self.index += 1;
                continue :state .comment;
            },
        },

        .assign => switch (self.buffer[self.index]) {
            '=' => {
                self.index += 1;
                result.range.end = self.index;
                result.tag = .eql;
            },

            '>' => {
                self.index += 1;
                result.range.end = self.index;
                result.tag = .fat_arrow;
            },

            else => {
                result.range.end = self.index;
            },
        },

        .bool_not => switch (self.buffer[self.index]) {
            '=' => {
                self.index += 1;
                result.range.end = self.index;
                result.tag = .not_eql;
            },

            else => {
                result.range.end = self.index;
            },
        },

        .period => switch (self.buffer[self.index]) {
            '.' => {
                result.range.start = self.index;
                result.tag = .double_period;
                self.index += 1;
                continue :state .double_period;
            },

            else => {
                result.range.end = self.index;
            },
        },

        .double_period => switch (self.buffer[self.index]) {
            '.' => {
                self.index += 1;
                result.range.end = self.index;
                result.tag = .triple_period;
            },

            else => {
                result.range.end = self.index;
            },
        },

        .less_than => switch (self.buffer[self.index]) {
            '<' => {
                self.index += 1;
                result.range.end = self.index;
                result.tag = .left_shift;
            },

            else => {
                result.range.end = self.index;
            },
        },

        .greater_than => switch (self.buffer[self.index]) {
            '>' => {
                self.index += 1;
                result.range.end = self.index;
                result.tag = .right_shift;
            },

            else => {
                result.range.end = self.index;
            },
        },
    }

    return result;
}
