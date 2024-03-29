const std = @import("std");

const Token = @import("Token.zig");
const Lexer = @import("Lexer.zig");
const Type = @import("type.zig").Type;

pub const Loc = struct {
    line: usize,
    column: usize,
};

pub const Symbol = struct {
    buffer: []const u8,
    loc: Loc,
};

pub const Root = struct {
    declarations: []const Declaration,
    loc: Loc, // EOF location
};

pub const Declaration = union(enum) {
    function: Function,

    pub const Function = struct {
        prototype: Prototype,
        body: []const Node,

        pub const Prototype = struct {
            name: Symbol,
            parameters: []const Parameter,
            return_type: Type,

            pub const Parameter = struct {
                name: Symbol,
                expected_type: Type,
            };
        };
    };
};

pub const Node = union(enum) {
    stmt: Stmt,
    expr: Expr,

    pub const Stmt = union(enum) {
        variable_declaration: VariableDeclaration,

        ret: Return,

        pub const VariableDeclaration = struct {
            name: Symbol,
            type: Type,
            value: Node.Expr,
        };

        pub const Return = struct {
            value: Expr,
            loc: Loc,
        };
    };

    pub const Expr = union(enum) {
        identifier: Identifier,
        string: String,
        char: Char,
        int: Int,
        float: Float,

        pub const Identifier = struct {
            symbol: Symbol,
        };

        pub const String = struct {
            value: []const u8,
            loc: Loc,
        };

        pub const Char = struct {
            value: u8,
            loc: Loc,
        };

        pub const Int = struct {
            value: i64,
            loc: Loc,
        };

        pub const Float = struct {
            value: f64,
            loc: Loc,
        };
    };
};

pub const Parser = struct {
    buffer: [:0]const u8,

    tokens: []const Token,
    current_token_index: usize,

    error_info: ?ErrorInfo = null,

    gpa: std.mem.Allocator,

    pub const Error = error{ UnexpectedToken, InvalidChar, InvalidNumber, InvalidType, ExpectedTopLevelDeclaration, Unsupported } || std.mem.Allocator.Error;

    pub const ErrorInfo = struct {
        message: []const u8,
        loc: Loc,
    };

    pub fn init(gpa: std.mem.Allocator, buffer: [:0]const u8) std.mem.Allocator.Error!Parser {
        var tokens = std.ArrayList(Token).init(gpa);

        var lexer = Lexer.init(buffer);

        while (true) {
            const token = lexer.next();

            try tokens.append(token);

            if (token.tag == .eof) break;
        }

        return Parser{ .buffer = buffer, .tokens = try tokens.toOwnedSlice(), .current_token_index = 0, .gpa = gpa };
    }

    fn nextToken(self: *Parser) Token {
        self.current_token_index += 1;

        return self.tokens[self.current_token_index - 1];
    }

    fn peekToken(self: Parser) Token {
        return self.tokens[self.current_token_index];
    }

    fn expectToken(self: *Parser, tag: Token.Tag) bool {
        if (self.peekToken().tag == tag) {
            _ = self.nextToken();

            return true;
        } else {
            return false;
        }
    }

    fn tokenValue(self: Parser, token: Token) []const u8 {
        return self.buffer[token.buffer_loc.start..token.buffer_loc.end];
    }

    fn tokenLoc(self: Parser, token: Token) Loc {
        var loc = Loc{ .line = 1, .column = 0 };

        for (0..self.buffer.len) |i| {
            switch (self.buffer[i]) {
                0 => break,

                '\n' => {
                    loc.line += 1;
                    loc.column = 1;
                },

                else => loc.column += 1,
            }

            if (i == token.buffer_loc.start) break;
        }

        return loc;
    }

    fn symbolFromToken(self: Parser, token: Token) Symbol {
        return Symbol{ .buffer = self.tokenValue(token), .loc = self.tokenLoc(token) };
    }

    pub fn parseRoot(self: *Parser) Error!Root {
        var declarations = std.ArrayList(Declaration).init(self.gpa);

        while (self.peekToken().tag != .eof) {
            try declarations.append(try self.parseDeclaration());
        }

        return Root{ .declarations = try declarations.toOwnedSlice(), .loc = self.tokenLoc(self.peekToken()) };
    }

    fn parseDeclaration(self: *Parser) Error!Declaration {
        switch (self.peekToken().tag) {
            .keyword_let => {
                self.error_info = .{ .message = "global variables is unspported feature", .loc = self.tokenLoc(self.peekToken()) };

                return error.Unsupported;
            },

            .keyword_fn => return self.parseFunctionDeclaration(),

            else => {
                self.error_info = .{ .message = "expected top level declaration", .loc = self.tokenLoc(self.peekToken()) };

                return error.ExpectedTopLevelDeclaration;
            },
        }
    }

    fn parseFunctionDeclaration(self: *Parser) Error!Declaration {
        _ = self.nextToken();

        const prototype = try self.parseFunctionPrototype();

        var body = std.ArrayList(Node).init(self.gpa);

        if (!self.expectToken(.open_brace)) {
            self.error_info = .{ .message = "expected '{'", .loc = self.tokenLoc(self.peekToken()) };

            return error.UnexpectedToken;
        }

        while (!self.expectToken(.close_brace)) {
            if (self.peekToken().tag == .eof) {
                self.error_info = .{ .message = "expected '}'", .loc = self.tokenLoc(self.peekToken()) };

                return error.UnexpectedToken;
            }

            try body.append(try self.parseStmt());
        }

        return Declaration{ .function = .{
            .prototype = prototype,
            .body = try body.toOwnedSlice(),
        } };
    }

    fn parseFunctionPrototype(self: *Parser) Error!Declaration.Function.Prototype {
        if (self.peekToken().tag != .identifier) {
            self.error_info = .{ .message = "expected function name", .loc = self.tokenLoc(self.peekToken()) };

            return error.UnexpectedToken;
        }

        const name = self.symbolFromToken(self.nextToken());

        const parameters = try self.parseFunctionParameters();

        const returnType = try self.parseType();

        return Declaration.Function.Prototype{ .name = name, .parameters = parameters, .return_type = returnType };
    }

    fn parseFunctionParameters(self: *Parser) Error![]const Declaration.Function.Prototype.Parameter {
        if (!self.expectToken(.open_paren)) {
            self.error_info = .{ .message = "expected '('", .loc = self.tokenLoc(self.peekToken()) };

            return error.UnexpectedToken;
        }

        var paramters = std.ArrayList(Declaration.Function.Prototype.Parameter).init(self.gpa);

        while (!self.expectToken(.close_paren)) {
            if (self.peekToken().tag == .eof) {
                self.error_info = .{ .message = "expected ')'", .loc = self.tokenLoc(self.peekToken()) };

                return error.UnexpectedToken;
            }

            try paramters.append(try self.parseFunctionParameter());

            if (!self.expectToken(.comma) and self.peekToken().tag != .close_paren) {
                self.error_info = .{ .message = "expected ',' after parameter", .loc = self.tokenLoc(self.peekToken()) };

                return error.UnexpectedToken;
            }
        }

        return try paramters.toOwnedSlice();
    }

    fn parseFunctionParameter(self: *Parser) Error!Declaration.Function.Prototype.Parameter {
        if (self.peekToken().tag != .identifier) {
            self.error_info = .{ .message = "expected parameter name", .loc = self.tokenLoc(self.peekToken()) };

            return error.UnexpectedToken;
        }

        const name = self.symbolFromToken(self.nextToken());

        const expected_type = try self.parseType();

        return Declaration.Function.Prototype.Parameter{
            .name = name,
            .expected_type = expected_type,
        };
    }

    fn parseStmt(self: *Parser) Error!Node {
        switch (self.peekToken().tag) {
            .keyword_let => {
                return self.parseVariableDeclarationStmt();
            },

            .keyword_return => return self.parseReturnStmt(),

            else => return self.parseExpr(),
        }
    }

    fn parseVariableDeclarationStmt(self: *Parser) Error!Node {
        _ = self.nextToken();

        if (self.peekToken().tag != .identifier) {
            self.error_info = .{ .message = "expected variable name", .loc = self.tokenLoc(self.peekToken()) };

            return error.UnexpectedToken;
        }

        const name = self.symbolFromToken(self.nextToken());

        const var_type = try self.parseType();

        if (!self.expectToken(.equal_sign)) {
            self.error_info = .{ .message = "expected '='", .loc = self.tokenLoc(self.peekToken()) };

            return error.UnexpectedToken;
        }

        const value = try self.parseExpr();

        return Node{ .stmt = .{ .variable_declaration = .{ .name = name, .type = var_type, .value = value.expr } } };
    }

    fn parseReturnStmt(self: *Parser) Error!Node {
        const keyword = self.nextToken();

        const value = try self.parseExpr();

        return Node{ .stmt = .{ .ret = .{ .value = value.expr, .loc = self.tokenLoc(keyword) } } };
    }

    fn parseExpr(self: *Parser) Error!Node {
        switch (self.peekToken().tag) {
            .identifier => {
                return Node{ .expr = .{ .identifier = .{ .symbol = self.symbolFromToken(self.nextToken()) } } };
            },

            .string_literal => {
                const literal_token = self.nextToken();

                return Node{ .expr = .{ .string = .{
                    .value = self.tokenValue(literal_token),
                    .loc = self.tokenLoc(literal_token),
                } } };
            },

            .char_literal => {
                if (self.tokenValue(self.peekToken()).len != 1) {
                    self.error_info = .{ .message = "invalid character literal: expected only one character", .loc = self.tokenLoc(self.peekToken()) };

                    return error.InvalidChar;
                }

                const literal_token = self.nextToken();

                return Node{ .expr = .{ .char = .{
                    .value = self.tokenValue(literal_token)[0],
                    .loc = self.tokenLoc(literal_token),
                } } };
            },

            .int => {
                const value = std.fmt.parseInt(i64, self.tokenValue(self.peekToken()), 10) catch {
                    self.error_info = .{ .message = "invalid number", .loc = self.tokenLoc(self.peekToken()) };

                    return error.InvalidNumber;
                };

                return Node{ .expr = .{ .int = .{ .value = value, .loc = self.tokenLoc(self.nextToken()) } } };
            },

            .float => {
                const value = std.fmt.parseFloat(f64, self.tokenValue(self.peekToken())) catch {
                    self.error_info = .{ .message = "invalid number", .loc = self.tokenLoc(self.peekToken()) };

                    return error.InvalidNumber;
                };

                return Node{ .expr = .{ .float = .{ .value = value, .loc = self.tokenLoc(self.nextToken()) } } };
            },

            else => {
                self.error_info = .{ .message = "expected expression", .loc = self.tokenLoc(self.peekToken()) };

                return error.UnexpectedToken;
            },
        }
    }

    fn parseType(self: *Parser) Error!Type {
        const BuiltinTypes = std.ComptimeStringMap(Type, .{ .{ "void", .void_type }, .{ "string", .string_type }, .{ "char", .char_type }, .{ "int", .int_type }, .{ "float", .float_type } });

        switch (self.peekToken().tag) {
            .identifier => {
                if (BuiltinTypes.get(self.tokenValue(self.peekToken()))) |builtinType| {
                    _ = self.nextToken();
                    return builtinType;
                } else {
                    self.error_info = .{ .message = "invalid type", .loc = self.tokenLoc(self.peekToken()) };

                    return error.InvalidType;
                }
            },

            else => {
                self.error_info = .{ .message = "invalid type", .loc = self.tokenLoc(self.peekToken()) };

                return error.InvalidType;
            },
        }
    }
};

test "parsing function declaration" {
    try testParse(
        \\fn main() int {
        \\  return 0
        \\}
    , .{ .declarations = &.{.{ .function = .{ .prototype = .{
        .name = .{ .buffer = "main", .loc = .{ .line = 1, .column = 4 } },
        .parameters = &.{},
        .return_type = .int_type,
    }, .body = &.{Node{ .stmt = .{ .ret = .{ .value = .{ .int = .{ .value = 0, .loc = .{ .line = 2, .column = 11 } } }, .loc = .{ .line = 2, .column = 4 } } } }} } }}, .loc = .{ .line = 3, .column = 1 } });
}

fn testParse(source: [:0]const u8, expected_root: Root) !void {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();

    var parser = try Parser.init(source, arena.allocator());

    const actual_root = try parser.parseRoot();

    try std.testing.expectEqual(expected_root.declarations.len, actual_root.declarations.len);

    for (expected_root.declarations, 0..) |expected_declaration, i| {
        const actual_declaration = actual_root.declarations[i];

        try std.testing.expectEqualDeep(expected_declaration, actual_declaration);
    }
}
