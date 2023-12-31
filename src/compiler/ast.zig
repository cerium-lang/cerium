const std = @import("std");

const Token = @import("lexer.zig").Token;
const Lexer = @import("lexer.zig").Lexer;
const Type = @import("typing.zig").Type;

pub const Root = struct {
    declarations: []const Declaration,
};

pub const Declaration = struct {
    kind: Kind,
    data: Data,

    pub const Kind = enum {
        function_declaration,
    };

    pub const Data = union {
        function: Function,

        pub const Function = struct {
            prototype: Prototype,
            body: []const Node,

            pub const Prototype = struct {
                name: []const u8,
                parameters: []const Parameter,
                return_type: Type,

                pub const Parameter = struct {
                    name: []const u8,
                    expected_type: Type,
                };
            };
        };
    };
};

pub const Node = struct {
    kind: Kind,
    data: Data,

    pub const Kind = enum { stmt, expr };

    pub const Data = union {
        stmt: Stmt,
        expr: Expr,

        pub const Stmt = union(enum) {
            ret: Return,

            pub const Return = struct {
                value: Expr,
            };
        };

        pub const Expr = union(enum) {
            string: String,
            char: Char,
            int: Int,
            float: Float,

            pub const String = struct {
                value: []const u8,
            };

            pub const Char = struct {
                value: u8,
            };

            pub const Int = struct {
                value: i64,
            };

            pub const Float = struct {
                value: f64,
            };
        };
    };
};

pub const Parser = struct {
    source: [:0]const u8,
    tokens: []const Token,
    current_token_index: usize,
    gpa: std.mem.Allocator,

    pub const Error = error{ UnexpectedToken, InvalidChar, InvalidNumber, InvalidType, ExpectedTopLevelDeclaration } || std.mem.Allocator.Error;

    pub fn init(source: [:0]const u8, gpa: std.mem.Allocator) std.mem.Allocator.Error!Parser {
        var tokens = std.ArrayList(Token).init(gpa);

        var lexer = Lexer.init(source);
        while (true) {
            const token = lexer.next();
            try tokens.append(token);

            if (token.kind == .eof) break;
        }

        return Parser{ .source = source, .tokens = try tokens.toOwnedSlice(), .current_token_index = 0, .gpa = gpa };
    }

    fn nextToken(self: *Parser) Token {
        self.current_token_index += 1;
        return self.tokens[self.current_token_index - 1];
    }

    fn peekToken(self: Parser) Token {
        return self.tokens[self.current_token_index];
    }

    fn expectToken(self: *Parser, kind: Token.Kind) bool {
        if (self.peekToken().kind == kind) {
            _ = self.nextToken();
            return true;
        } else {
            return false;
        }
    }

    fn tokenValue(self: Parser, token: Token) []const u8 {
        return self.source[token.loc.start..token.loc.end];
    }

    pub fn parseRoot(self: *Parser) Error!Root {
        var declarations = std.ArrayList(Declaration).init(self.gpa);

        while (self.peekToken().kind != .eof) {
            try declarations.append(try self.parseDeclaration());
        }

        return Root{ .declarations = try declarations.toOwnedSlice() };
    }

    fn parseDeclaration(self: *Parser) Error!Declaration {
        switch (self.peekToken().kind) {
            .keyword_fn => return self.parseFunctionDeclaration(),
            else => return Error.ExpectedTopLevelDeclaration,
        }
    }

    fn parseFunctionDeclaration(self: *Parser) Error!Declaration {
        _ = self.nextToken();

        const prototype = try self.parseFunctionPrototype();

        var body = std.ArrayList(Node).init(self.gpa);

        if (!self.expectToken(.open_brace)) {
            return Error.UnexpectedToken;
        }

        while (!self.expectToken(.close_brace)) {
            if (self.peekToken().kind == .eof) {
                return Error.UnexpectedToken;
            }

            try body.append(try self.parseStmt());
        }

        return Declaration{ .kind = .function_declaration, .data = .{ .function = .{
            .prototype = prototype,
            .body = try body.toOwnedSlice(),
        } } };
    }

    fn parseFunctionPrototype(self: *Parser) Error!Declaration.Data.Function.Prototype {
        if (self.peekToken().kind != .identifier) {
            return Error.UnexpectedToken;
        }

        const name = self.tokenValue(self.nextToken());

        const parameters = try self.parseFunctionParameters();

        const returnType = try self.parseType();

        return Declaration.Data.Function.Prototype{ .name = name, .parameters = parameters, .return_type = returnType };
    }

    fn parseFunctionParameters(self: *Parser) Error![]const Declaration.Data.Function.Prototype.Parameter {
        if (!self.expectToken(.open_paren)) {
            return Error.UnexpectedToken;
        }

        var paramters = std.ArrayList(Declaration.Data.Function.Prototype.Parameter).init(self.gpa);

        while (!self.expectToken(.close_paren)) {
            if (self.peekToken().kind == .eof) {
                return Error.UnexpectedToken;
            }

            try paramters.append(try self.parseFunctionParameter());
        }

        return try paramters.toOwnedSlice();
    }

    fn parseFunctionParameter(self: *Parser) Error!Declaration.Data.Function.Prototype.Parameter {
        if (self.peekToken().kind != .identifier) {
            return Error.UnexpectedToken;
        }

        const name = self.tokenValue(self.nextToken());

        const expectedType = try self.parseType();

        return Declaration.Data.Function.Prototype.Parameter{
            .name = name,
            .expected_type = expectedType,
        };
    }

    fn parseStmt(self: *Parser) Error!Node {
        switch (self.peekToken().kind) {
            .keyword_return => return self.parseReturnStmt(),
            else => return self.parseExpr(),
        }
    }

    fn parseReturnStmt(self: *Parser) Error!Node {
        _ = self.nextToken();

        const value = try self.parseExpr();

        return Node{ .kind = .stmt, .data = .{ .stmt = .{ .ret = .{ .value = value.data.expr } } } };
    }

    fn parseExpr(self: *Parser) Error!Node {
        switch (self.peekToken().kind) {
            .string_literal => {
                return Node{ .kind = .expr, .data = .{ .expr = .{ .string = .{
                    .value = self.tokenValue(self.nextToken()),
                } } } };
            },
            .char_literal => {
                if (self.tokenValue(self.peekToken()).len != 1) {
                    return Error.InvalidChar;
                }

                return Node{ .kind = .expr, .data = .{ .expr = .{ .char = .{
                    .value = self.tokenValue(self.nextToken())[0],
                } } } };
            },
            .number => {
                var is_float = false;

                for (self.tokenValue(self.peekToken())) |char| {
                    if (char == '.') is_float = true;
                }

                if (is_float) {
                    const value = std.fmt.parseFloat(f64, self.tokenValue(self.peekToken())) catch {
                        return Error.InvalidNumber;
                    };

                    _ = self.nextToken();

                    return Node{ .kind = .expr, .data = .{ .expr = .{ .float = .{ .value = value } } } };
                } else {
                    const value = std.fmt.parseInt(i64, self.tokenValue(self.peekToken()), 10) catch {
                        return Error.InvalidNumber;
                    };

                    _ = self.nextToken();

                    return Node{ .kind = .expr, .data = .{ .expr = .{ .int = .{ .value = value } } } };
                }
            },
            else => return Error.UnexpectedToken,
        }
    }

    fn parseType(self: *Parser) Error!Type {
        const BuiltinTypes = std.ComptimeStringMap(Type, .{ .{ "void", .void_type }, .{ "string", .string_type }, .{ "char", .char_type }, .{ "int", .int_type }, .{ "float", .float_type } });

        switch (self.peekToken().kind) {
            .identifier => {
                if (BuiltinTypes.get(self.tokenValue(self.peekToken()))) |builtinType| {
                    _ = self.nextToken();
                    return builtinType;
                } else {
                    return Error.InvalidType;
                }
            },
            else => return Error.InvalidType,
        }
    }
};

test "parsing function declaration" {
    try testParser(
        \\fn main() int {
        \\  return 0
        \\}
    , .{ .declarations = &.{.{ .kind = .function_declaration, .data = .{ .function = .{ .prototype = .{
        .name = "main",
        .parameters = &.{},
        .return_type = .int_type,
    }, .body = &.{Node{ .kind = .stmt, .data = .{ .stmt = .{ .ret = .{ .value = .{ .int = .{ .value = 0 } } } } } }} } } }} });
}

fn testParser(source: [:0]const u8, expected_root: Root) !void {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();

    var parser = try Parser.init(source, arena.allocator());

    const actual_root = try parser.parseRoot();

    try std.testing.expectEqual(expected_root.declarations.len, actual_root.declarations.len);

    for (expected_root.declarations, 0..) |expected_declaration, i| {
        const actual_declaration = actual_root.declarations[i];

        try std.testing.expectEqual(expected_declaration.kind, actual_declaration.kind);

        switch (expected_declaration.kind) {
            .function_declaration => {
                try std.testing.expectEqualDeep(expected_declaration.data.function.prototype, actual_declaration.data.function.prototype);
                try std.testing.expectEqualDeep(expected_declaration.data.function.body.len, actual_declaration.data.function.body.len);

                for (expected_declaration.data.function.body, 0..) |expected_node, j| {
                    const actual_node = actual_declaration.data.function.body[j];

                    try std.testing.expectEqual(expected_node.kind, actual_node.kind);

                    switch (expected_node.kind) {
                        .stmt => {
                            try std.testing.expectEqualDeep(expected_node.data.stmt, actual_node.data.stmt);
                        },
                        .expr => {
                            try std.testing.expectEqualDeep(expected_node.data.expr, actual_node.data.expr);
                        },
                    }
                }
            },
        }
    }
}
