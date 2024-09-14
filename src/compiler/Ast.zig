//! Abstract Syntax Tree.
//!
//! A tree that represents how the syntax is expressed, mainly just a step before lowering to HIR.

const std = @import("std");

const Compilation = @import("Compilation.zig");
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
            is_const: bool,
            name: Name,
            type: ?Type,
            value: Node.Expr,
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
        assembly: Assembly,
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

        pub const Assembly = struct {
            content: []const u8,
            input_constraints: []const InputConstraint,
            output_constraint: ?OutputConstraint,
            source_loc: SourceLoc,

            const InputConstraint = struct {
                register: []const u8,
                value: *const Expr,
            };

            const OutputConstraint = struct {
                register: []const u8,
                type: Type,
            };
        };

        pub const UnaryOperation = struct {
            operator: Operator,
            rhs: *const Expr,
            source_loc: SourceLoc,

            pub const Operator = enum {
                minus,
                bang,
                tilde,
                ampersand,
                star,
            };
        };

        pub const BinaryOperation = struct {
            lhs: *const Expr,
            operator: Operator,
            rhs: *const Expr,
            source_loc: SourceLoc,

            pub const Operator = enum {
                plus,
                minus,
                star,
                forward_slash,
                less_than,
                double_less_than,
                greater_than,
                double_greater_than,
                ampersand,
                pipe,
                caret,
                equal_sign,
                double_equal_sign,
                bang_equal_sign,
            };
        };

        pub const Call = struct {
            callable: *const Expr,
            arguments: []const Expr,
            source_loc: SourceLoc,
        };

        pub fn getSourceLoc(self: Expr) SourceLoc {
            return switch (self) {
                .identifier => |identifier| identifier.name.source_loc,

                inline else => |other| other.source_loc,
            };
        }
    };
};

pub const Parser = struct {
    allocator: std.mem.Allocator,

    builtin_types: std.StringHashMapUnmanaged(Type),

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

    pub fn init(allocator: std.mem.Allocator, compilation: Compilation, buffer: [:0]const u8) std.mem.Allocator.Error!Parser {
        var tokens: std.ArrayListUnmanaged(Token) = .{};

        var lexer = Lexer.init(buffer);

        while (true) {
            const token = lexer.next();

            try tokens.append(allocator, token);

            if (token.tag == .eof) break;
        }

        var builtin_types: std.StringHashMapUnmanaged(Type) = .{};

        try builtin_types.ensureTotalCapacity(allocator, 14);

        builtin_types.putAssumeCapacity("void", .{ .tag = .void });
        builtin_types.putAssumeCapacity("u8", .{ .tag = .u8 });
        builtin_types.putAssumeCapacity("u16", .{ .tag = .u16 });
        builtin_types.putAssumeCapacity("u32", .{ .tag = .u32 });
        builtin_types.putAssumeCapacity("u64", .{ .tag = .u64 });
        builtin_types.putAssumeCapacity("usize", Type.makeInt(false, compilation.env.target.ptrBitWidth()));
        builtin_types.putAssumeCapacity("i8", .{ .tag = .i8 });
        builtin_types.putAssumeCapacity("i16", .{ .tag = .i16 });
        builtin_types.putAssumeCapacity("i32", .{ .tag = .i32 });
        builtin_types.putAssumeCapacity("i64", .{ .tag = .i64 });
        builtin_types.putAssumeCapacity("isize", Type.makeInt(true, compilation.env.target.ptrBitWidth()));
        builtin_types.putAssumeCapacity("f32", .{ .tag = .f32 });
        builtin_types.putAssumeCapacity("f64", .{ .tag = .f64 });
        builtin_types.putAssumeCapacity("bool", .{ .tag = .bool });

        return Parser{
            .allocator = allocator,
            .builtin_types = builtin_types,
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

            .keyword_const, .keyword_var => try self.parseVariableDeclarationStmt(),

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
        const is_const = self.nextToken().tag == .keyword_const;

        const name = try self.parseName();

        const @"type" = if (self.eatToken(.equal_sign))
            null
        else
            try self.parseType();

        if (@"type" != null and !self.eatToken(.equal_sign)) {
            self.error_info = .{ .message = "expected '='", .source_loc = self.tokenSourceLoc(self.peekToken()) };

            return error.UnexpectedToken;
        }

        const value = try self.parseExpr(.lowest);

        return Node{
            .stmt = .{
                .variable_declaration = .{
                    .is_const = is_const,
                    .name = name,
                    .type = @"type",
                    .value = value.expr,
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
        assign,
        comparison,
        sum,
        product,
        bit_or,
        bit_xor,
        bit_and,
        shift,
        prefix,
        call,

        fn from(token: Token) Precedence {
            return switch (token.tag) {
                .equal_sign => .assign,
                .less_than, .greater_than, .double_equal_sign, .bang_equal_sign => .comparison,
                .plus, .minus => .sum,
                .star, .forward_slash => .product,
                .ampersand => .bit_and,
                .pipe => .bit_or,
                .caret => .bit_xor,
                .double_less_than, .double_greater_than => .shift,
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

            .keyword_asm => return self.parseAssemblyExpr(),

            .minus => return self.parseUnaryOperationExpr(.minus),
            .bang => return self.parseUnaryOperationExpr(.bang),
            .tilde => return self.parseUnaryOperationExpr(.tilde),
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

    fn parseAssemblyExpr(self: *Parser) Error!Node.Expr {
        const asm_keyword_token = self.nextToken();

        var content: std.ArrayListUnmanaged(u8) = .{};

        if (!self.eatToken(.open_brace)) {
            self.error_info = .{ .message = "expected a '{'", .source_loc = self.tokenSourceLoc(self.peekToken()) };

            return error.UnexpectedToken;
        }

        var input_constraints: []const Node.Expr.Assembly.InputConstraint = &.{};
        var output_constraint: ?Node.Expr.Assembly.OutputConstraint = null;

        while (!self.eatToken(.close_brace)) {
            if (self.peekToken().tag == .eof) {
                self.error_info = .{ .message = "expected a '}'", .source_loc = self.tokenSourceLoc(self.peekToken()) };

                return error.UnexpectedToken;
            }

            if (self.peekToken().tag != .string_literal) {
                self.error_info = .{ .message = "expected a valid string", .source_loc = self.tokenSourceLoc(self.peekToken()) };

                return error.UnexpectedToken;
            }

            try content.appendSlice(self.allocator, (try self.parseStringExpr()).string.value);

            var parsed_constraints = false;

            if (self.eatToken(.colon)) {
                input_constraints = try self.parseAssemblyInputConstraints();
                parsed_constraints = true;
            }

            if (self.eatToken(.colon)) {
                output_constraint = try self.parseAssemblyOutputConstraint();
            }

            if (self.peekToken().tag != .close_brace) {
                if (parsed_constraints) {
                    self.error_info = .{ .message = "expected a '}'", .source_loc = self.tokenSourceLoc(self.peekToken()) };

                    return error.UnexpectedToken;
                } else {
                    try content.append(self.allocator, '\n');
                }
            }
        }

        return Node.Expr{
            .assembly = .{
                .content = try content.toOwnedSlice(self.allocator),
                .input_constraints = input_constraints,
                .output_constraint = output_constraint,
                .source_loc = self.tokenSourceLoc(asm_keyword_token),
            },
        };
    }

    fn parseAssemblyInputConstraints(self: *Parser) Error![]Node.Expr.Assembly.InputConstraint {
        var constraints = std.ArrayList(Node.Expr.Assembly.InputConstraint).init(self.allocator);

        while (self.peekToken().tag != .eof and self.peekToken().tag != .colon and self.peekToken().tag != .close_brace) {
            try constraints.append(try self.parseAssemblyInputConstraint());

            if (!self.eatToken(.comma) and self.peekToken().tag != .colon and self.peekToken().tag != .close_brace) {
                self.error_info = .{ .message = "expected a ','", .source_loc = self.tokenSourceLoc(self.peekToken()) };

                return error.UnexpectedToken;
            }
        }

        return constraints.toOwnedSlice();
    }

    fn parseAssemblyInputConstraint(self: *Parser) Error!Node.Expr.Assembly.InputConstraint {
        if (self.peekToken().tag != .string_literal) {
            self.error_info = .{ .message = "expected a valid string", .source_loc = self.tokenSourceLoc(self.peekToken()) };

            return error.UnexpectedToken;
        }

        const register = (try self.parseStringExpr()).string.value;

        if (!self.eatToken(.open_paren)) {
            self.error_info = .{ .message = "expected a '('", .source_loc = self.tokenSourceLoc(self.peekToken()) };

            return error.UnexpectedToken;
        }

        const value = (try self.parseExpr(.lowest)).expr;

        if (!self.eatToken(.close_paren)) {
            self.error_info = .{ .message = "expected a ')'", .source_loc = self.tokenSourceLoc(self.peekToken()) };

            return error.UnexpectedToken;
        }

        const value_on_heap = try self.allocator.create(Node.Expr);
        value_on_heap.* = value;

        return Node.Expr.Assembly.InputConstraint{
            .register = register,
            .value = value_on_heap,
        };
    }

    fn parseAssemblyOutputConstraint(self: *Parser) Error!Node.Expr.Assembly.OutputConstraint {
        if (self.peekToken().tag != .string_literal) {
            self.error_info = .{ .message = "expected a valid string", .source_loc = self.tokenSourceLoc(self.peekToken()) };

            return error.UnexpectedToken;
        }

        const register = (try self.parseStringExpr()).string.value;

        if (!self.eatToken(.open_paren)) {
            self.error_info = .{ .message = "expected a '('", .source_loc = self.tokenSourceLoc(self.peekToken()) };

            return error.UnexpectedToken;
        }

        const @"type" = try self.parseType();

        if (!self.eatToken(.close_paren)) {
            self.error_info = .{ .message = "expected a ')'", .source_loc = self.tokenSourceLoc(self.peekToken()) };

            return error.UnexpectedToken;
        }

        return Node.Expr.Assembly.OutputConstraint{
            .register = register,
            .type = @"type",
        };
    }

    fn parseUnaryOperationExpr(self: *Parser, comptime operator: Node.Expr.UnaryOperation.Operator) Error!Node.Expr {
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
            .less_than => return self.parseBinaryOperationExpr(lhs, .less_than),
            .double_less_than => return self.parseBinaryOperationExpr(lhs, .double_less_than),
            .greater_than => return self.parseBinaryOperationExpr(lhs, .greater_than),
            .double_greater_than => return self.parseBinaryOperationExpr(lhs, .double_greater_than),
            .ampersand => return self.parseBinaryOperationExpr(lhs, .ampersand),
            .pipe => return self.parseBinaryOperationExpr(lhs, .pipe),
            .caret => return self.parseBinaryOperationExpr(lhs, .caret),
            .equal_sign => return self.parseBinaryOperationExpr(lhs, .equal_sign),
            .double_equal_sign => return self.parseBinaryOperationExpr(lhs, .double_equal_sign),
            .bang_equal_sign => return self.parseBinaryOperationExpr(lhs, .bang_equal_sign),

            .open_paren => return self.parseCallExpr(lhs),

            else => {
                self.error_info = .{ .message = "unexpected token", .source_loc = self.tokenSourceLoc(self.peekToken()) };

                return error.UnexpectedToken;
            },
        }
    }

    fn parseBinaryOperationExpr(self: *Parser, lhs: Node.Expr, comptime operator: Node.Expr.BinaryOperation.Operator) Error!Node.Expr {
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
        switch (self.peekToken().tag) {
            .identifier => {
                if (self.builtin_types.get(self.tokenValue(self.peekToken()))) |builtin_type| {
                    _ = self.nextToken();

                    return builtin_type;
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

            else => {},
        }

        self.error_info = .{ .message = "invalid type", .source_loc = self.tokenSourceLoc(self.peekToken()) };

        return error.InvalidType;
    }
};
