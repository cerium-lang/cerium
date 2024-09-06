//! Abstract Syntax Tree.
//!
//! A tree that represents how the syntax is expressed, mainly just a step before lowering to HIR.

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
        assembly: Assembly,
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

        pub const Assembly = struct {
            content: []const u8,
            source_loc: SourceLoc,
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
        unary_operation: UnaryOperation,
        binary_operation: BinaryOperation,

        pub const Identifier = struct {
            name: Name,
        };

        pub const String = struct {
            value: []const u8,
            source_loc: SourceLoc,
        };

        pub const Int = struct {
            value: i128,
            source_loc: SourceLoc,
        };

        pub const Float = struct {
            value: f64,
            source_loc: SourceLoc,
        };

        pub const UnaryOperation = struct {
            operator: Operator,
            rhs: *Expr,
            source_loc: SourceLoc,

            pub const Operator = enum {
                minus,
            };
        };

        pub const BinaryOperation = struct {
            lhs: *Expr,
            operator: Operator,
            rhs: *Expr,
            source_loc: SourceLoc,

            pub const Operator = enum {
                plus,
                minus,
                star,
                forward_slash,
            };
        };

        pub fn getSourceLoc(self: Expr) SourceLoc {
            return switch (self) {
                .identifier => |identifier| identifier.name.source_loc,
                .string => |string| string.source_loc,
                .int => |int| int.source_loc,
                .float => |float| float.source_loc,
                .unary_operation => |unary_operation| unary_operation.source_loc,
                .binary_operation => |binary_operation| binary_operation.source_loc,
            };
        }
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

            .keyword_asm => self.parseAssemblyStmt(),

            .keyword_return => self.parseReturnStmt(),

            else => self.parseExpr(.lowest),
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

        const value = try self.parseExpr(.lowest);

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

    fn parseAssemblyStmt(self: *Parser) Error!Node {
        _ = self.nextToken();

        const token = self.nextToken();

        if (token.tag != .string_literal) {
            self.error_info = .{ .message = "expected the content of assembly to be a string literal", .source_loc = self.tokenSourceLoc(token) };

            return error.UnexpectedToken;
        }

        return Node{
            .stmt = .{
                .assembly = .{
                    .content = self.tokenValue(token),
                    .source_loc = self.tokenSourceLoc(token),
                },
            },
        };
    }

    fn parseReturnStmt(self: *Parser) Error!Node {
        const keyword = self.nextToken();

        const value = try self.parseExpr(.lowest);

        return Node{
            .stmt = .{
                .@"return" = .{
                    .value = value.expr,
                    .source_loc = self.tokenSourceLoc(keyword),
                },
            },
        };
    }

    const Precedence = enum {
        lowest,
        sum,
        product,
        prefix,

        fn from(token: Token) Precedence {
            return switch (token.tag) {
                .plus, .minus => .sum,
                .star, .forward_slash => .product,

                else => .lowest,
            };
        }
    };

    fn parseExpr(self: *Parser, precedence: Precedence) Error!Node {
        var lhs = try self.parseUnaryExpr();

        while (@intFromEnum(Precedence.from(self.peekToken())) > @intFromEnum(precedence)) {
            lhs = try self.parseBinaryExpr(lhs);
        }

        return Node{ .expr = lhs };
    }

    fn parseUnaryExpr(self: *Parser) Error!Node.Expr {
        switch (self.peekToken().tag) {
            .identifier => return self.parseIdentifierExpr(),

            .string_literal => return self.parseStringExpr(),

            .char_literal => return self.parseCharExpr(),

            .int => return self.parseIntExpr(),

            .float => return self.parseFloatExpr(),

            .minus => return self.parseUnaryOperationExpr(.minus),

            else => {
                self.error_info = .{ .message = "unexpected token", .source_loc = self.tokenSourceLoc(self.peekToken()) };

                return error.UnexpectedToken;
            },
        }
    }

    fn parseIdentifierExpr(self: *Parser) Error!Node.Expr {
        return Node.Expr{
            .identifier = .{
                .name = try self.parseName(),
            },
        };
    }

    fn parseStringExpr(self: *Parser) Error!Node.Expr {
        return Node.Expr{
            .string = .{
                .value = self.tokenValue(self.peekToken()),
                .source_loc = self.tokenSourceLoc(self.nextToken()),
            },
        };
    }

    fn parseCharExpr(self: *Parser) Error!Node.Expr {
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

        return Node.Expr{
            .int = .{
                .value = decoded,
                .source_loc = self.tokenSourceLoc(token),
            },
        };
    }

    fn parseIntExpr(self: *Parser) Error!Node.Expr {
        const value = std.fmt.parseInt(i128, self.tokenValue(self.peekToken()), 10) catch {
            self.error_info = .{ .message = "invalid number", .source_loc = self.tokenSourceLoc(self.peekToken()) };

            return error.InvalidNumber;
        };

        return Node.Expr{
            .int = .{
                .value = value,
                .source_loc = self.tokenSourceLoc(self.nextToken()),
            },
        };
    }

    fn parseFloatExpr(self: *Parser) Error!Node.Expr {
        const value = std.fmt.parseFloat(f64, self.tokenValue(self.peekToken())) catch {
            self.error_info = .{ .message = "invalid number", .source_loc = self.tokenSourceLoc(self.peekToken()) };

            return error.InvalidNumber;
        };

        return Node.Expr{
            .float = .{
                .value = value,
                .source_loc = self.tokenSourceLoc(self.nextToken()),
            },
        };
    }

    fn parseUnaryOperationExpr(self: *Parser, operator: Node.Expr.UnaryOperation.Operator) Error!Node.Expr {
        const operator_token = self.nextToken();

        const rhs = (try self.parseExpr(.prefix)).expr;
        const rhs_on_heap = try self.allocator.create(Node.Expr);
        rhs_on_heap.* = rhs;

        return Node.Expr{ .unary_operation = .{ .operator = operator, .rhs = rhs_on_heap, .source_loc = self.tokenSourceLoc(operator_token) } };
    }

    fn parseBinaryExpr(self: *Parser, lhs: Node.Expr) Error!Node.Expr {
        switch (self.peekToken().tag) {
            .plus => return self.parseBinaryOperationExpr(lhs, .plus),
            .minus => return self.parseBinaryOperationExpr(lhs, .minus),
            .star => return self.parseBinaryOperationExpr(lhs, .star),
            .forward_slash => return self.parseBinaryOperationExpr(lhs, .forward_slash),

            else => {
                self.error_info = .{ .message = "unexpected token", .source_loc = self.tokenSourceLoc(self.peekToken()) };

                return error.UnexpectedToken;
            },
        }
    }

    fn parseBinaryOperationExpr(self: *Parser, lhs: Node.Expr, operator: Node.Expr.BinaryOperation.Operator) Error!Node.Expr {
        const lhs_on_heap = try self.allocator.create(Node.Expr);
        lhs_on_heap.* = lhs;

        const operator_token = self.nextToken();

        const rhs = (try self.parseExpr(Precedence.from(operator_token))).expr;
        const rhs_on_heap = try self.allocator.create(Node.Expr);
        rhs_on_heap.* = rhs;

        return Node.Expr{ .binary_operation = .{ .lhs = lhs_on_heap, .operator = operator, .rhs = rhs_on_heap, .source_loc = self.tokenSourceLoc(operator_token) } };
    }

    fn parseType(self: *Parser) Error!Type {
        const builtin_types = std.StaticStringMap(Type).initComptime(
            .{
                .{ "void", .{ .tag = .void } },
                .{ "string", .{ .tag = .string } },
                .{ "u8", .{ .tag = .u8 } },
                .{ "u16", .{ .tag = .u16 } },
                .{ "u32", .{ .tag = .u32 } },
                .{ "u64", .{ .tag = .u64 } },
                .{ "i8", .{ .tag = .i8 } },
                .{ "i16", .{ .tag = .i16 } },
                .{ "i32", .{ .tag = .i32 } },
                .{ "i64", .{ .tag = .i64 } },
                .{ "f32", .{ .tag = .f32 } },
                .{ "f64", .{ .tag = .f64 } },
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
