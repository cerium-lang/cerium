const std = @import("std");

const Token = @import("Token.zig");
const Lexer = @import("Lexer.zig");
const Type = @import("Type.zig");

const Ast = @This();

body: []const Node,

pub const SourceLoc = struct {
    line: usize,
    column: usize,
};

pub const Name = struct {
    buffer: []const u8,
    source_loc: SourceLoc,
};

pub const Node = union(enum) {
    stmt: Stmt,
    expr: Expr,

    pub const Stmt = union(enum) {
        function_declaration: FunctionDeclaration,
        variable_declaration: VariableDeclaration,
        inline_assembly: InlineAssembly,
        @"return": Return,

        pub const FunctionDeclaration = struct {
            prototype: Prototype,
            body: []const Node,

            pub const Prototype = struct {
                name: Name,
                parameters: []const Parameter,
                return_type: Type,

                pub const Parameter = struct {
                    name: Name,
                    expected_type: Type,
                };
            };
        };

        pub const VariableDeclaration = struct {
            name: Name,
            type: Type,
            value: Node.Expr,
        };

        pub const InlineAssembly = struct {
            content: []const u8,
        };

        pub const Return = struct {
            value: Expr,
            source_loc: SourceLoc,
        };
    };

    pub const Expr = union(enum) {
        identifier: Identifier,
        string: String,
        int: Int,
        float: Float,

        pub const Identifier = struct {
            name: Name,
        };

        pub const String = struct {
            value: []const u8,
            source_loc: SourceLoc,
        };

        pub const Int = struct {
            value: i64,
            source_loc: SourceLoc,
        };

        pub const Float = struct {
            value: f64,
            source_loc: SourceLoc,
        };
    };
};

pub const Parser = struct {
    allocator: std.mem.Allocator,

    buffer: [:0]const u8,

    tokens: []const Token,
    current_token_index: usize,

    error_info: ?ErrorInfo = null,

    pub const Error = error{
        UnexpectedToken,
        InvalidChar,
        InvalidNumber,
        InvalidType,
        ExpectedTopLevelDeclaration,
    } || std.mem.Allocator.Error;

    pub const ErrorInfo = struct {
        message: []const u8,
        source_loc: SourceLoc,
    };

    pub fn init(allocator: std.mem.Allocator, buffer: [:0]const u8) std.mem.Allocator.Error!Parser {
        var tokens = std.ArrayList(Token).init(allocator);

        var lexer = Lexer.init(buffer);

        while (true) {
            const token = lexer.next();

            try tokens.append(token);

            if (token.tag == .eof) break;
        }

        return Parser{
            .allocator = allocator,
            .buffer = buffer,
            .tokens = try tokens.toOwnedSlice(),
            .current_token_index = 0,
        };
    }

    pub fn parse(self: *Parser) Error!Ast {
        var body = std.ArrayList(Node).init(self.allocator);

        while (self.peekToken().tag != .eof) {
            try body.append(try self.parseStmt());
        }

        return Ast{ .body = try body.toOwnedSlice() };
    }

    fn nextToken(self: *Parser) Token {
        self.current_token_index += 1;

        return self.tokens[self.current_token_index - 1];
    }

    fn peekToken(self: Parser) Token {
        return self.tokens[self.current_token_index];
    }

    fn eatToken(self: *Parser, tag: Token.Tag) bool {
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

    fn tokenSourceLoc(self: Parser, token: Token) SourceLoc {
        var source_loc = SourceLoc{ .line = 1, .column = 0 };

        for (0..self.buffer.len) |i| {
            switch (self.buffer[i]) {
                0 => break,

                '\n' => {
                    source_loc.line += 1;
                    source_loc.column = 1;
                },

                else => source_loc.column += 1,
            }

            if (i == token.buffer_loc.start) break;
        }

        return source_loc;
    }

    fn parseName(self: *Parser) Error!Name {
        if (self.peekToken().tag != .identifier) {
            self.error_info = .{ .message = "expected a name", .source_loc = self.tokenSourceLoc(self.peekToken()) };

            return error.UnexpectedToken;
        }

        const token = self.nextToken();

        return Name{ .buffer = self.tokenValue(token), .source_loc = self.tokenSourceLoc(token) };
    }

    fn parseFunctionDeclarationStmt(self: *Parser) Error!Node {
        _ = self.nextToken();

        const prototype = try self.parseFunctionPrototype();

        const body = try self.parseBody();

        return Node{
            .stmt = .{
                .function_declaration = .{
                    .prototype = prototype,
                    .body = body,
                },
            },
        };
    }

    fn parseFunctionPrototype(self: *Parser) Error!Node.Stmt.FunctionDeclaration.Prototype {
        const name = try self.parseName();

        const parameters = try self.parseFunctionParameters();

        const returnType = try self.parseType();

        return Node.Stmt.FunctionDeclaration.Prototype{ .name = name, .parameters = parameters, .return_type = returnType };
    }

    fn parseFunctionParameters(self: *Parser) Error![]Node.Stmt.FunctionDeclaration.Prototype.Parameter {
        if (!self.eatToken(.open_paren)) {
            self.error_info = .{ .message = "expected '('", .source_loc = self.tokenSourceLoc(self.peekToken()) };

            return error.UnexpectedToken;
        }

        var paramters = std.ArrayList(Node.Stmt.FunctionDeclaration.Prototype.Parameter).init(self.allocator);

        while (!self.eatToken(.close_paren)) {
            if (self.peekToken().tag == .eof) {
                self.error_info = .{ .message = "expected ')'", .source_loc = self.tokenSourceLoc(self.peekToken()) };

                return error.UnexpectedToken;
            }

            try paramters.append(try self.parseFunctionParameter());

            if (!self.eatToken(.comma) and self.peekToken().tag != .close_paren) {
                self.error_info = .{ .message = "expected ',' after parameter", .source_loc = self.tokenSourceLoc(self.peekToken()) };

                return error.UnexpectedToken;
            }
        }

        return try paramters.toOwnedSlice();
    }

    fn parseFunctionParameter(self: *Parser) Error!Node.Stmt.FunctionDeclaration.Prototype.Parameter {
        return Node.Stmt.FunctionDeclaration.Prototype.Parameter{
            .name = try self.parseName(),
            .expected_type = try self.parseType(),
        };
    }

    fn parseBody(self: *Parser) Error![]Node {
        var body = std.ArrayList(Node).init(self.allocator);

        if (!self.eatToken(.open_brace)) {
            self.error_info = .{ .message = "expected '{'", .source_loc = self.tokenSourceLoc(self.peekToken()) };

            return error.UnexpectedToken;
        }

        while (!self.eatToken(.close_brace)) {
            if (self.peekToken().tag == .eof) {
                self.error_info = .{ .message = "expected '}'", .source_loc = self.tokenSourceLoc(self.peekToken()) };

                return error.UnexpectedToken;
            }

            try body.append(try self.parseStmt());
        }

        return body.toOwnedSlice();
    }

    fn parseStmt(self: *Parser) Error!Node {
        return switch (self.peekToken().tag) {
            .keyword_let => self.parseVariableDeclarationStmt(),

            .keyword_fn => return self.parseFunctionDeclarationStmt(),

            .keyword_asm => self.parseInlineAssemblyStmt(),

            .keyword_return => self.parseReturnStmt(),

            else => self.parseExpr(),
        };
    }

    fn parseVariableDeclarationStmt(self: *Parser) Error!Node {
        _ = self.nextToken();

        const name = try self.parseName();

        const var_type = try self.parseType();

        if (!self.eatToken(.equal_sign)) {
            self.error_info = .{ .message = "expected '='", .source_loc = self.tokenSourceLoc(self.peekToken()) };

            return error.UnexpectedToken;
        }

        const value = try self.parseExpr();

        return Node{
            .stmt = .{
                .variable_declaration = .{
                    .name = name,
                    .type = var_type,
                    .value = value.expr,
                },
            },
        };
    }

    fn parseInlineAssemblyStmt(self: *Parser) Error!Node {
        _ = self.nextToken();

        if (self.peekToken().tag != .string_literal) {
            self.error_info = .{ .message = "expected the content of inline assembly to be a string literal", .source_loc = self.tokenSourceLoc(self.peekToken()) };

            return error.UnexpectedToken;
        }

        const content = self.tokenValue(self.nextToken());

        return Node{
            .stmt = .{
                .inline_assembly = .{
                    .content = content,
                },
            },
        };
    }

    fn parseReturnStmt(self: *Parser) Error!Node {
        const keyword = self.nextToken();

        const value = try self.parseExpr();

        return Node{
            .stmt = .{
                .@"return" = .{
                    .value = value.expr,
                    .source_loc = self.tokenSourceLoc(keyword),
                },
            },
        };
    }

    fn parseExpr(self: *Parser) Error!Node {
        switch (self.peekToken().tag) {
            .identifier => return self.parseIdentifierExpr(),

            .string_literal => return self.parseStringExpr(),

            .char_literal => return self.parseCharExpr(),

            .int => return self.parseIntExpr(),

            .float => return self.parseFloatExpr(),

            else => {
                self.error_info = .{ .message = "unexpected token", .source_loc = self.tokenSourceLoc(self.peekToken()) };

                return error.UnexpectedToken;
            },
        }
    }

    fn parseIdentifierExpr(self: *Parser) Error!Node {
        return Node{ .expr = .{ .identifier = .{ .name = try self.parseName() } } };
    }

    fn parseStringExpr(self: *Parser) Error!Node {
        return Node{
            .expr = .{
                .string = .{
                    .value = self.tokenValue(self.peekToken()),
                    .source_loc = self.tokenSourceLoc(self.nextToken()),
                },
            },
        };
    }

    fn parseCharExpr(self: *Parser) Error!Node {
        const token = self.nextToken();
        const encoded = self.tokenValue(token);
        const location = self.tokenSourceLoc(token);

        const decoded = switch (encoded.len) {
            1 => encoded[0],
            2 => std.unicode.utf8Decode2(encoded[0..2].*),
            3 => std.unicode.utf8Decode3(encoded[0..3].*),
            4 => std.unicode.utf8Decode4(encoded[0..4].*),
            else => error.TooMuchCodes,
        } catch {
            self.error_info = .{ .message = "invalid character literal", .source_loc = location };

            return error.InvalidChar;
        };

        return Node{
            .expr = .{
                .int = .{
                    .value = decoded,
                    .source_loc = self.tokenSourceLoc(token),
                },
            },
        };
    }

    fn parseIntExpr(self: *Parser) Error!Node {
        const value = std.fmt.parseInt(i64, self.tokenValue(self.peekToken()), 10) catch {
            self.error_info = .{ .message = "invalid number", .source_loc = self.tokenSourceLoc(self.peekToken()) };

            return error.InvalidNumber;
        };

        return Node{ .expr = .{ .int = .{ .value = value, .source_loc = self.tokenSourceLoc(self.nextToken()) } } };
    }

    fn parseFloatExpr(self: *Parser) Error!Node {
        const value = std.fmt.parseFloat(f64, self.tokenValue(self.peekToken())) catch {
            self.error_info = .{ .message = "invalid number", .source_loc = self.tokenSourceLoc(self.peekToken()) };

            return error.InvalidNumber;
        };

        return Node{ .expr = .{ .float = .{ .value = value, .source_loc = self.tokenSourceLoc(self.nextToken()) } } };
    }

    fn parseType(self: *Parser) Error!Type {
        const builtin_types = std.StaticStringMap(Type).initComptime(
            .{
                .{ "void", .{ .tag = .void_type } },
                .{ "string", .{ .tag = .string_type } },
                .{ "u8", .{ .tag = .u8_type } },
                .{ "u16", .{ .tag = .u16_type } },
                .{ "u32", .{ .tag = .u32_type } },
                .{ "u64", .{ .tag = .u64_type } },
                .{ "i8", .{ .tag = .i8_type } },
                .{ "i16", .{ .tag = .i16_type } },
                .{ "i32", .{ .tag = .i32_type } },
                .{ "i64", .{ .tag = .i64_type } },
                .{ "f32", .{ .tag = .f32_type } },
                .{ "f64", .{ .tag = .f64_type } },
            },
        );

        switch (self.peekToken().tag) {
            .identifier => {
                if (builtin_types.get(self.tokenValue(self.peekToken()))) |builtin_type| {
                    _ = self.nextToken();

                    return builtin_type;
                } else {
                    self.error_info = .{ .message = "invalid type", .source_loc = self.tokenSourceLoc(self.peekToken()) };

                    return error.InvalidType;
                }
            },

            else => {
                self.error_info = .{ .message = "invalid type", .source_loc = self.tokenSourceLoc(self.peekToken()) };

                return error.InvalidType;
            },
        }
    }
};
