const std = @import("std");

pub const Token = struct {
    kind: Kind,
    loc: Loc,

    pub const Kind = enum { eof, invalid, identifier, string_literal, char_literal, number, open_paren, close_paren, open_brace, close_brace };

    pub const Loc = struct {
        start: usize,
        end: usize,
    };
};

pub const Lexer = struct {
    buffer: [:0]const u8,
    index: usize,
    state: State,

    pub const State = enum {
        start,
        identifier,
        string_literal,
        char_literal,
        number,
    };

    pub fn init(buffer: [:0]const u8) Lexer {
        return Lexer{ .buffer = buffer, .index = 0, .state = .start };
    }

    pub fn next(self: *Lexer) Token {
        var result = Token{ .kind = .eof, .loc = .{ .start = self.index, .end = self.index } };

        while (self.buffer.len >= self.index) : (self.index += 1) {
            const current_char = self.buffer[self.index];

            switch (self.state) {
                .start => switch (current_char) {
                    0 => break,

                    ' ', '\r', '\n', '\t' => {},

                    'a'...'z', 'A'...'Z', '_' => {
                        result.loc.start = self.index;
                        result.kind = .identifier;
                        self.state = .identifier;
                    },

                    '"' => {
                        result.loc.start = self.index + 1;
                        result.kind = .string_literal;
                        self.state = .string_literal;
                    },

                    '\'' => {
                        result.loc.start = self.index + 1;
                        result.kind = .char_literal;
                        self.state = .char_literal;
                    },

                    '0'...'9' => {
                        result.loc.start = self.index;
                        result.kind = .number;
                        self.state = .number;
                    },

                    '(' => {
                        result.loc.start = self.index;
                        self.index += 1;
                        result.loc.end = self.index;
                        result.kind = .open_paren;
                        break;
                    },

                    ')' => {
                        result.loc.start = self.index;
                        self.index += 1;
                        result.loc.end = self.index;
                        result.kind = .close_paren;
                        break;
                    },

                    '{' => {
                        result.loc.start = self.index;
                        self.index += 1;
                        result.loc.end = self.index;
                        result.kind = .open_brace;
                        break;
                    },

                    '}' => {
                        result.loc.start = self.index;
                        self.index += 1;
                        result.loc.end = self.index;
                        result.kind = .close_brace;
                        break;
                    },

                    else => {
                        result.loc.start = self.index;
                        self.index += 1;
                        result.loc.end = self.index;
                        result.kind = .invalid;
                        break;
                    },
                },

                .identifier => switch (current_char) {
                    'a'...'z', 'A'...'Z', '0'...'9', '_' => {},

                    else => {
                        result.loc.end = self.index;
                        self.state = .start;
                        break;
                    },
                },

                .string_literal => switch (current_char) {
                    0 => {
                        result.loc.end = self.index;
                        self.state = .start;
                        result.kind = .invalid;
                        break;
                    },

                    '\n' => {
                        result.loc.end = self.index;
                        self.index += 1;
                        self.state = .start;
                        result.kind = .invalid;
                        break;
                    },

                    '"' => {
                        result.loc.end = self.index;
                        self.index += 1;
                        self.state = .start;
                        break;
                    },

                    else => {},
                },

                .char_literal => switch (current_char) {
                    0 => {
                        result.loc.end = self.index;
                        self.state = .start;
                        result.kind = .invalid;
                        break;
                    },

                    '\n' => {
                        result.loc.end = self.index;
                        self.index += 1;
                        self.state = .start;
                        result.kind = .invalid;
                        break;
                    },

                    '\'' => {
                        result.loc.end = self.index;
                        self.index += 1;
                        self.state = .start;
                        break;
                    },

                    else => {},
                },

                .number => switch (current_char) {
                    '0'...'9', '.' => {},

                    else => {
                        result.loc.end = self.index;
                        self.state = .start;
                        break;
                    },
                },
            }
        }

        return result;
    }
};

test "valid identifiers" {
    try testTokenize("identifier another_1d3ntifier AndAnotherIdentifierAlso THAT_IS_AN_IDENTIFIER_BTW", &.{ .identifier, .identifier, .identifier, .identifier });
}

test "valid numbers" {
    try testTokenize("10 1.0 2.0 0.5 55.0 6.0 7", &.{ .number, .number, .number, .number, .number, .number, .number });
}

test "valid string literals" {
    try testTokenize(
        \\"You can type anything you want"
    , &.{.string_literal});
}

test "valid char literals" {
    try testTokenize(
        \\'y' 's'
    , &.{ .char_literal, .char_literal });
}

test "invalid string literals" {
    try testTokenize(
        \\"invalid string
        \\"
    , &.{ .invalid, .invalid });
}

test "invalid char literals" {
    try testTokenize(
        \\'i
        \\'
    , &.{ .invalid, .invalid });
}

test "invalid tokens" {
    try testTokenize("@ & % $", &.{ .invalid, .invalid, .invalid, .invalid });
}

test "unicode" {
    // This should work fine, but since many terminals does not support UTF-8, it may not show up correctly.
    try testTokenize("\"🔥\"", &.{.string_literal});
}

fn testTokenize(buffer: [:0]const u8, expected_token_kinds: []const Token.Kind) !void {
    var lexer = Lexer.init(buffer);

    for (expected_token_kinds) |expected_token_kind| {
        const token = lexer.next();

        std.debug.print("\n{s}\n", .{buffer[token.loc.start..token.loc.end]});
        std.debug.print("\n{}\n", .{token});

        try std.testing.expectEqual(expected_token_kind, token.kind);
    }

    const eof_token = lexer.next();

    try std.testing.expectEqual(Token.Kind.eof, eof_token.kind);
    try std.testing.expectEqual(buffer.len, eof_token.loc.start);
    try std.testing.expectEqual(buffer.len, eof_token.loc.end);
}
