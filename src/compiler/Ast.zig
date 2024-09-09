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
        call: Call,

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
                ampersand,
                star,
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

        pub const Call = struct {
            callable: *Expr,
            arguments: []const Expr,
            source_loc: SourceLoc,
        };

        pub fn getSourceLoc(self: Expr) SourceLoc {
            return switch (self) {
                .identifier => |identifier| identifier.name.source_loc,
                .string => |string| string.source_loc,
                .int => |int| int.source_loc,
                .float => |float| float.source_loc,
                .unary_operation => |unary_operation| unary_operation.source_loc,
                .binary_operation => |binary_operation| binary_operation.source_loc,
                .call => |call| call.source_loc,
            };
        }
    };
};

pub const Parser = struct {
    allocator: std.mem.Allocator,

    buffer: [:0]const u8,

    tokens: []const Token,
    current_token_index: usize,

    in_function: bool = false,

    error_info: ?ErrorInfo = null,

    pub const Error = error{
        UnexpectedToken,
        InvalidString,
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
        var tokens: std.ArrayListUnmanaged(Token) = .{};

        var lexer = Lexer.init(buffer);

        while (true) {
            const token = lexer.next();

            try tokens.append(allocator, token);

            if (token.tag == .eof) break;
        }

        return Parser{
            .allocator = allocator,
            .buffer = buffer,
            .tokens = try tokens.toOwnedSlice(allocator),
            .current_token_index = 0,
        };
    }

    pub fn parse(self: *Parser) Error!Ast {
        var body: std.ArrayListUnmanaged(Node) = .{};

        while (self.peekToken().tag != .eof) {
            try body.append(self.allocator, try self.parseStmt());
        }

        return Ast{ .body = try body.toOwnedSlice(self.allocator) };
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

    fn parseStmt(self: *Parser) Error!Node {
        const node = switch (self.peekToken().tag) {
            .keyword_fn => return self.parseFunctionDeclarationStmt(),

            .keyword_let => try self.parseVariableDeclarationStmt(),

            .keyword_asm => try self.parseAssemblyStmt(),

            .keyword_return => try self.parseReturnStmt(),

            else => try self.parseExpr(.lowest),
        };

        if (!self.eatToken(.semicolon)) {
            self.error_info = .{ .message = "expected ';'", .source_loc = self.tokenSourceLoc(self.peekToken()) };

            return error.UnexpectedToken;
        }

        return node;
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

        const was_in_function = self.in_function;
        self.in_function = true;

        const body = try self.parseBody();

        self.in_function = was_in_function;

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

        const return_type = try self.parseType();

        return Node.Stmt.FunctionDeclaration.Prototype{ .name = name, .parameters = parameters, .return_type = return_type };
    }

    fn parseFunctionParameters(self: *Parser) Error![]Node.Stmt.FunctionDeclaration.Prototype.Parameter {
        if (!self.eatToken(.open_paren)) {
            self.error_info = .{ .message = "expected '('", .source_loc = self.tokenSourceLoc(self.peekToken()) };

            return error.UnexpectedToken;
        }

        var paramters: std.ArrayListUnmanaged(Node.Stmt.FunctionDeclaration.Prototype.Parameter) = .{};

        while (!self.eatToken(.close_paren)) {
            if (self.peekToken().tag == .eof) {
                self.error_info = .{ .message = "expected ')'", .source_loc = self.tokenSourceLoc(self.peekToken()) };

                return error.UnexpectedToken;
            }

            try paramters.append(self.allocator, try self.parseFunctionParameter());

            if (!self.eatToken(.comma) and self.peekToken().tag != .close_paren) {
                self.error_info = .{ .message = "expected ',' after parameter", .source_loc = self.tokenSourceLoc(self.peekToken()) };

                return error.UnexpectedToken;
            }
        }

        return paramters.toOwnedSlice(self.allocator);
    }

    fn parseFunctionParameter(self: *Parser) Error!Node.Stmt.FunctionDeclaration.Prototype.Parameter {
        return Node.Stmt.FunctionDeclaration.Prototype.Parameter{
            .name = try self.parseName(),
            .expected_type = try self.parseType(),
        };
    }

    fn parseBody(self: *Parser) Error![]Node {
        var body: std.ArrayListUnmanaged(Node) = .{};

        if (!self.eatToken(.open_brace)) {
            self.error_info = .{ .message = "expected '{'", .source_loc = self.tokenSourceLoc(self.peekToken()) };

            return error.UnexpectedToken;
        }

        while (!self.eatToken(.close_brace)) {
            if (self.peekToken().tag == .eof) {
                self.error_info = .{ .message = "expected '}'", .source_loc = self.tokenSourceLoc(self.peekToken()) };

                return error.UnexpectedToken;
            }

            try body.append(self.allocator, try self.parseStmt());
        }

        return body.toOwnedSlice(self.allocator);
    }

    fn parseVariableDeclarationStmt(self: *Parser) Error!Node {
        _ = self.nextToken();

        const name = try self.parseName();

        const variable_type = try self.parseType();

        if (!self.eatToken(.equal_sign)) {
            self.error_info = .{ .message = "expected '='", .source_loc = self.tokenSourceLoc(self.peekToken()) };

            return error.UnexpectedToken;
        }

        const value = try self.parseExpr(.lowest);

        return Node{
            .stmt = .{
                .variable_declaration = .{
                    .name = name,
                    .type = variable_type,
                    .value = value.expr,
                },
            },
        };
    }

    fn parseAssemblyStmt(self: *Parser) Error!Node {
        const asm_keyword_token = self.nextToken();

        var content: std.ArrayListUnmanaged(u8) = .{};

        if (!self.eatToken(.open_brace)) {
            self.error_info = .{ .message = "expected '{'", .source_loc = self.tokenSourceLoc(self.peekToken()) };

            return error.UnexpectedToken;
        }

        while (!self.eatToken(.close_brace)) {
            if (self.peekToken().tag == .eof) {
                self.error_info = .{ .message = "expected '}'", .source_loc = self.tokenSourceLoc(self.peekToken()) };

                return error.UnexpectedToken;
            }

            if (self.peekToken().tag != .string_literal) {
                self.error_info = .{ .message = "expected the content of assembly to be a string literal", .source_loc = self.tokenSourceLoc(self.peekToken()) };

                return error.UnexpectedToken;
            }

            const parsed_string = try self.parseStringExpr();

            try content.appendSlice(self.allocator, parsed_string.string.value);

            if (self.peekToken().tag != .close_brace) try content.append(self.allocator, '\n');
        }

        return Node{
            .stmt = .{
                .assembly = .{
                    .content = try content.toOwnedSlice(self.allocator),
                    .source_loc = self.tokenSourceLoc(asm_keyword_token),
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
        call,

        fn from(token: Token) Precedence {
            return switch (token.tag) {
                .plus, .minus => .sum,
                .star, .forward_slash => .product,
                .open_paren => .call,

                else => .lowest,
            };
        }
    };

    fn parseExpr(self: *Parser, precedence: Precedence) Error!Node {
        var lhs = try self.parseUnaryExpr();

        while (@intFromEnum(Precedence.from(self.peekToken())) > @intFromEnum(precedence) and self.peekToken().tag != .semicolon) {
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
            .ampersand => return self.parseUnaryOperationExpr(.ampersand),
            .star => return self.parseUnaryOperationExpr(.star),

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
        const content = self.tokenValue(self.peekToken());
        const source_loc = self.tokenSourceLoc(self.nextToken());

        var unescaped = try std.ArrayListUnmanaged(u8).initCapacity(self.allocator, content.len);

        var unescaping = false;

        for (content) |char| {
            switch (unescaping) {
                false => switch (char) {
                    '\\' => unescaping = true,

                    else => unescaped.appendAssumeCapacity(char),
                },

                true => {
                    unescaping = false;

                    switch (char) {
                        '\\' => {
                            unescaped.appendAssumeCapacity('\\');
                        },

                        'n' => {
                            unescaped.appendAssumeCapacity('\n');
                        },

                        'r' => {
                            unescaped.appendAssumeCapacity('\r');
                        },

                        't' => {
                            unescaped.appendAssumeCapacity('\t');
                        },

                        'e' => {
                            unescaped.appendAssumeCapacity(27);
                        },

                        'v' => {
                            unescaped.appendAssumeCapacity(11);
                        },

                        'b' => {
                            unescaped.appendAssumeCapacity(8);
                        },

                        'f' => {
                            unescaped.appendAssumeCapacity(20);
                        },

                        '"' => {
                            unescaped.appendAssumeCapacity('"');
                        },

                        else => {
                            self.error_info = .{ .message = "invalid escape character in string", .source_loc = source_loc };

                            return error.InvalidString;
                        },
                    }
                },
            }
        }

        return Node.Expr{
            .string = .{
                .value = unescaped.items,
                .source_loc = source_loc,
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
        const value = std.fmt.parseInt(i128, self.tokenValue(self.peekToken()), 0) catch {
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

            .open_paren => return self.parseCallExpr(lhs),

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

        return Node.Expr{
            .binary_operation = .{
                .lhs = lhs_on_heap,
                .operator = operator,
                .rhs = rhs_on_heap,
                .source_loc = self.tokenSourceLoc(operator_token),
            },
        };
    }

    fn parseCallExpr(self: *Parser, callable: Node.Expr) Error!Node.Expr {
        const callable_on_heap = try self.allocator.create(Node.Expr);
        callable_on_heap.* = callable;

        const open_paren_token = self.nextToken();

        const arguments = try self.parseCallArguments();

        return Node.Expr{
            .call = .{
                .callable = callable_on_heap,
                .arguments = arguments,
                .source_loc = self.tokenSourceLoc(open_paren_token),
            },
        };
    }

    fn parseCallArguments(self: *Parser) Error![]Node.Expr {
        var arguments = std.ArrayList(Node.Expr).init(self.allocator);

        while (self.peekToken().tag != .eof and self.peekToken().tag != .close_paren) {
            try arguments.append((try self.parseExpr(.lowest)).expr);

            if (!self.eatToken(.comma) and self.peekToken().tag != .close_paren) {
                self.error_info = .{ .message = "expected a ','", .source_loc = self.tokenSourceLoc(self.peekToken()) };

                return error.UnexpectedToken;
            }
        }

        if (!self.eatToken(.close_paren)) {
            self.error_info = .{ .message = "expected a ')'", .source_loc = self.tokenSourceLoc(self.peekToken()) };

            return error.UnexpectedToken;
        }

        return arguments.items;
    }

    fn parseType(self: *Parser) Error!Type {
        const builtin_types = std.StaticStringMap(Type).initComptime(
            .{
                .{ "void", .{ .tag = .void } },
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

            .star => {
                _ = self.nextToken();

                const is_const = self.eatToken(.keyword_const);

                const child = try self.parseType();

                const child_on_heap = try self.allocator.create(Type);
                child_on_heap.* = child;

                return .{
                    .tag = .pointer,
                    .data = .{
                        .pointer = .{
                            .size = .one,
                            .is_const = is_const,
                            .is_local = self.in_function,
                            .child_type = child_on_heap,
                        },
                    },
                };
            },

            .open_bracket => {
                _ = self.nextToken();

                if (!self.eatToken(.star)) {
                    self.error_info = .{ .message = "expected a '*'", .source_loc = self.tokenSourceLoc(self.peekToken()) };

                    return error.UnexpectedToken;
                }

                if (!self.eatToken(.close_bracket)) {
                    self.error_info = .{ .message = "expected a ']'", .source_loc = self.tokenSourceLoc(self.peekToken()) };

                    return error.UnexpectedToken;
                }

                const is_const = self.eatToken(.keyword_const);

                const child = try self.parseType();

                const child_on_heap = try self.allocator.create(Type);
                child_on_heap.* = child;

                return .{
                    .tag = .pointer,
                    .data = .{
                        .pointer = .{
                            .size = .many,
                            .is_const = is_const,
                            .is_local = self.in_function,
                            .child_type = child_on_heap,
                        },
                    },
                };
            },

            else => {
                self.error_info = .{ .message = "invalid type", .source_loc = self.tokenSourceLoc(self.peekToken()) };

                return error.InvalidType;
            },
        }
    }
};
