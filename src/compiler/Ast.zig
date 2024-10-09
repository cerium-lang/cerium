//! Abstract Syntax Tree.
//!
//! A tree that represents how the syntax is expressed, mainly just a step before lowering to HIR.

const std = @import("std");

const Compilation = @import("Compilation.zig");
const Token = @import("Token.zig");
const Lexer = @import("Lexer.zig");
const Type = @import("Symbol.zig").Type;

const Ast = @This();

body: []const Node,

pub const SourceLoc = struct {
    line: usize = 1,
    column: usize = 1,
};

pub const Name = struct {
    buffer: []const u8,
    source_loc: SourceLoc,
};

pub const SubType = union(enum) {
    name: Name,
    function: Function,
    pointer: Pointer,
    pure: Type,

    pub const Function = struct {
        parameter_subtypes: []const SubType,
        return_subtype: *const SubType,
    };

    pub const Pointer = struct {
        size: Type.Pointer.Size,
        is_const: bool,
        is_local: bool,
        child_subtype: *const SubType,
    };
};

pub const Node = union(enum) {
    stmt: Stmt,
    expr: Expr,

    pub const Stmt = union(enum) {
        function_declaration: FunctionDeclaration,
        variable_declaration: VariableDeclaration,
        type_alias: TypeAlias,
        conditional: Conditional,
        while_loop: WhileLoop,
        @"break": Break,
        @"continue": Continue,
        @"return": Return,

        pub const FunctionDeclaration = struct {
            prototype: Prototype,
            body: []const Node,

            pub const Prototype = struct {
                is_external: bool,
                name: Name,
                parameters: []const Parameter,
                return_subtype: SubType,

                pub const Parameter = struct {
                    name: Name,
                    expected_subtype: SubType,
                };
            };
        };

        pub const VariableDeclaration = struct {
            is_external: bool,
            is_const: bool,
            name: Name,
            subtype: ?SubType,
            value: Node.Expr,
        };

        pub const TypeAlias = struct {
            name: Name,
            subtype: SubType,
        };

        pub const Conditional = struct {
            conditions: []const Expr,
            possiblities: []const []const Node,
            fallback: []const Node,
        };

        pub const WhileLoop = struct {
            condition: Expr,
            body: []Node,
        };

        pub const Break = struct {
            source_loc: SourceLoc,
        };

        pub const Continue = struct {
            source_loc: SourceLoc,
        };

        pub const Return = struct {
            maybe_value: ?Expr,
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
        subscript: Subscript,
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
                subtype: SubType,
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

        pub const Subscript = struct {
            target: *const Expr,
            index: *const Expr,
            source_loc: SourceLoc,
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

    buffer: [:0]const u8,

    tokens: []Token,
    current_token_index: usize,

    in_function: bool = false,

    error_info: ?ErrorInfo = null,

    pub const Error = error{
        UnexpectedToken,
        InvalidType,
        InvalidString,
        InvalidChar,
        InvalidNumber,
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

    pub fn deinit(self: *Parser) void {
        self.allocator.free(self.tokens);
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
        var source_loc: SourceLoc = .{};

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
            .keyword_extern => try self.parseExternStmt(),

            .keyword_fn => return self.parseFunctionDeclarationStmt(false),

            .keyword_const, .keyword_var => try self.parseVariableDeclarationStmt(false),

            .keyword_type => try self.parseTypeAliasStmt(),

            .keyword_if => return self.parseConditionalStmt(),

            .keyword_while => return self.parseWhileLoopStmt(),

            .keyword_break => self.parseBreakStmt(),

            .keyword_continue => self.parseContinueStmt(),

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

    fn parseExternStmt(self: *Parser) Error!Node {
        const source_loc = self.tokenSourceLoc(self.nextToken());

        if (self.peekToken().tag == .keyword_fn) {
            return self.parseFunctionDeclarationStmt(true);
        } else if (self.peekToken().tag == .keyword_var) {
            return self.parseVariableDeclarationStmt(true);
        } else if (self.peekToken().tag == .keyword_const) {
            self.error_info = .{ .message = "'const' is declaring a compile time constant and cannot be used with 'extern'", .source_loc = self.tokenSourceLoc(self.peekToken()) };

            return error.UnexpectedToken;
        } else {
            self.error_info = .{ .message = "expected a function or variable declaration", .source_loc = source_loc };

            return error.UnexpectedToken;
        }
    }

    fn parseFunctionDeclarationStmt(self: *Parser, is_external: bool) Error!Node {
        _ = self.nextToken();

        const prototype = try self.parseFunctionPrototype(is_external);

        if (is_external) {
            return Node{
                .stmt = .{
                    .function_declaration = .{
                        .prototype = prototype,
                        .body = &.{},
                    },
                },
            };
        }

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

    fn parseFunctionPrototype(self: *Parser, is_external: bool) Error!Node.Stmt.FunctionDeclaration.Prototype {
        const name = try self.parseName();

        const parameters = try self.parseFunctionParameters();

        const return_subtype = if (self.peekToken().tag == .open_brace or (self.peekToken().tag == .semicolon and is_external))
            SubType{ .pure = .void }
        else
            try self.parseSubType();

        return Node.Stmt.FunctionDeclaration.Prototype{
            .is_external = is_external,
            .name = name,
            .parameters = parameters,
            .return_subtype = return_subtype,
        };
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
            .expected_subtype = try self.parseSubType(),
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

    fn parseVariableDeclarationStmt(self: *Parser, is_external: bool) Error!Node {
        const init_token = self.nextToken();

        const is_const = init_token.tag == .keyword_const;

        const name = try self.parseName();

        const subtype = if (self.peekToken().tag == .equal_sign)
            null
        else
            try self.parseSubType();

        if (subtype != null and !is_external and !self.eatToken(.equal_sign)) {
            self.error_info = .{ .message = "expected '='", .source_loc = self.tokenSourceLoc(self.peekToken()) };

            return error.UnexpectedToken;
        } else if (self.eatToken(.equal_sign) and is_external) {
            self.error_info = .{ .message = "'extern' variables cannot have an initializer", .source_loc = self.tokenSourceLoc(self.peekToken()) };

            return error.UnexpectedToken;
        }

        const value = if (is_external) undefined else try self.parseExpr(.lowest);

        return Node{
            .stmt = .{
                .variable_declaration = .{
                    .is_external = is_external,
                    .is_const = is_const,
                    .name = name,
                    .subtype = subtype,
                    .value = if (is_external) undefined else value.expr,
                },
            },
        };
    }

    fn parseTypeAliasStmt(self: *Parser) Error!Node {
        _ = self.nextToken();

        const name = try self.parseName();

        if (!self.eatToken(.equal_sign)) {
            self.error_info = .{ .message = "expected '='", .source_loc = self.tokenSourceLoc(self.peekToken()) };

            return error.UnexpectedToken;
        }

        const subtype = try self.parseSubType();

        return Node{
            .stmt = .{
                .type_alias = .{
                    .name = name,
                    .subtype = subtype,
                },
            },
        };
    }

    fn parseConditionalStmt(self: *Parser) Error!Node {
        var conditions = std.ArrayList(Node.Expr).init(self.allocator);
        var possiblities = std.ArrayList([]Node).init(self.allocator);

        while (self.peekToken().tag == .keyword_if) {
            _ = self.nextToken();

            const condition = (try self.parseExpr(.lowest)).expr;
            const possibility = try self.parseBody();
            try conditions.append(condition);
            try possiblities.append(possibility);

            if (self.eatToken(.keyword_else)) {
                if (self.peekToken().tag == .keyword_if) continue;

                const fallback = try self.parseBody();

                return Node{ .stmt = .{ .conditional = .{ .conditions = try conditions.toOwnedSlice(), .possiblities = try possiblities.toOwnedSlice(), .fallback = fallback } } };
            }

            break;
        }

        return Node{ .stmt = .{ .conditional = .{ .conditions = try conditions.toOwnedSlice(), .possiblities = try possiblities.toOwnedSlice(), .fallback = &.{} } } };
    }

    fn parseWhileLoopStmt(self: *Parser) Error!Node {
        _ = self.nextToken();

        const condition = (try self.parseExpr(.lowest)).expr;
        const body = try self.parseBody();

        return Node{ .stmt = .{ .while_loop = .{ .condition = condition, .body = body } } };
    }

    fn parseBreakStmt(self: *Parser) Error!Node {
        const break_token = self.nextToken();

        return Node{ .stmt = .{ .@"break" = .{ .source_loc = self.tokenSourceLoc(break_token) } } };
    }

    fn parseContinueStmt(self: *Parser) Error!Node {
        const continue_token = self.nextToken();

        return Node{ .stmt = .{ .@"continue" = .{ .source_loc = self.tokenSourceLoc(continue_token) } } };
    }

    fn parseReturnStmt(self: *Parser) Error!Node {
        const return_token = self.nextToken();

        const maybe_value = if (self.peekToken().tag == .semicolon) null else (try self.parseExpr(.lowest)).expr;

        return Node{
            .stmt = .{
                .@"return" = .{
                    .maybe_value = maybe_value,
                    .source_loc = self.tokenSourceLoc(return_token),
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
        subscript,

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
                .open_bracket => .subscript,

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

            .open_paren => return self.parseParenthesesExpr(),

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

    const UnescapeError = error{InvalidEscapeCharacter} || std.mem.Allocator.Error;

    fn unescape(output: *std.ArrayList(u8), input: []const u8) UnescapeError!void {
        try output.ensureTotalCapacity(input.len);

        var unescaping = false;

        for (input) |input_char| {
            switch (unescaping) {
                false => switch (input_char) {
                    '\\' => unescaping = true,

                    else => output.appendAssumeCapacity(input_char),
                },

                true => {
                    unescaping = false;

                    switch (input_char) {
                        '\\' => {
                            output.appendAssumeCapacity('\\');
                        },

                        'n' => {
                            output.appendAssumeCapacity('\n');
                        },

                        'r' => {
                            output.appendAssumeCapacity('\r');
                        },

                        't' => {
                            output.appendAssumeCapacity('\t');
                        },

                        'e' => {
                            output.appendAssumeCapacity(27);
                        },

                        'v' => {
                            output.appendAssumeCapacity(11);
                        },

                        'b' => {
                            output.appendAssumeCapacity(8);
                        },

                        'f' => {
                            output.appendAssumeCapacity(20);
                        },

                        '"' => {
                            output.appendAssumeCapacity('"');
                        },

                        else => return error.InvalidEscapeCharacter,
                    }
                },
            }
        }
    }

    fn parseStringExpr(self: *Parser) Error!Node.Expr {
        const content = self.tokenValue(self.peekToken());
        const source_loc = self.tokenSourceLoc(self.nextToken());

        var unescaped_list = std.ArrayList(u8).init(self.allocator);

        unescape(&unescaped_list, content) catch |err| switch (err) {
            error.InvalidEscapeCharacter => {
                self.error_info = .{ .message = "invalid escape character", .source_loc = source_loc };

                return error.InvalidString;
            },

            inline else => |other_err| return other_err,
        };

        const unescaped = try unescaped_list.toOwnedSlice();

        return Node.Expr{
            .string = .{
                .value = unescaped,
                .source_loc = source_loc,
            },
        };
    }

    fn parseCharExpr(self: *Parser) Error!Node.Expr {
        const encoded = self.tokenValue(self.peekToken());
        const source_loc = self.tokenSourceLoc(self.nextToken());

        var unescaped_list = std.ArrayList(u8).init(self.allocator);

        unescape(&unescaped_list, encoded) catch |err| switch (err) {
            error.InvalidEscapeCharacter => {
                self.error_info = .{ .message = "invalid escape character", .source_loc = source_loc };

                return error.InvalidChar;
            },

            inline else => |other_err| return other_err,
        };

        const unescaped = try unescaped_list.toOwnedSlice();

        const decoded = switch (unescaped.len) {
            1 => unescaped[0],
            2 => std.unicode.utf8Decode2(unescaped[0..2].*),
            3 => std.unicode.utf8Decode3(unescaped[0..3].*),
            4 => std.unicode.utf8Decode4(unescaped[0..4].*),
            else => error.TooMuchCodes,
        } catch {
            self.error_info = .{ .message = "invalid character literal", .source_loc = source_loc };

            return error.InvalidChar;
        };

        return Node.Expr{
            .int = .{
                .value = decoded,
                .source_loc = source_loc,
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

            var parsed_input_constraints = false;

            if (self.eatToken(.colon)) {
                input_constraints = try self.parseAssemblyInputConstraints();
                parsed_input_constraints = true;
            }

            if (self.eatToken(.colon)) {
                output_constraint = try self.parseAssemblyOutputConstraint();
            }

            try content.append(self.allocator, '\n');

            if (self.peekToken().tag != .close_brace and parsed_input_constraints) {
                self.error_info = .{ .message = "expected a '}'", .source_loc = self.tokenSourceLoc(self.peekToken()) };

                return error.UnexpectedToken;
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

        const subtype = try self.parseSubType();

        if (!self.eatToken(.close_paren)) {
            self.error_info = .{ .message = "expected a ')'", .source_loc = self.tokenSourceLoc(self.peekToken()) };

            return error.UnexpectedToken;
        }

        return Node.Expr.Assembly.OutputConstraint{
            .register = register,
            .subtype = subtype,
        };
    }

    fn parseParenthesesExpr(self: *Parser) Error!Node.Expr {
        _ = self.nextToken();

        const value = (try self.parseExpr(.lowest)).expr;

        if (!self.eatToken(.close_paren)) {
            self.error_info = .{ .message = "expected a ')'", .source_loc = self.tokenSourceLoc(self.peekToken()) };

            return error.UnexpectedToken;
        }

        return value;
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

            .open_bracket => return self.parseSubscriptExpr(lhs),

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

    fn parseSubscriptExpr(self: *Parser, lhs: Node.Expr) Error!Node.Expr {
        const lhs_on_heap = try self.allocator.create(Node.Expr);
        lhs_on_heap.* = lhs;

        const open_bracket_token = self.nextToken();

        const rhs = (try self.parseExpr(.lowest)).expr;
        const rhs_on_heap = try self.allocator.create(Node.Expr);
        rhs_on_heap.* = rhs;

        if (!self.eatToken(.close_bracket)) {
            self.error_info = .{ .message = "expected a ']'", .source_loc = self.tokenSourceLoc(self.peekToken()) };

            return error.UnexpectedToken;
        }

        return Node.Expr{ .subscript = .{ .target = lhs_on_heap, .index = rhs_on_heap, .source_loc = self.tokenSourceLoc(open_bracket_token) } };
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

        return arguments.toOwnedSlice();
    }

    fn parseSubType(self: *Parser) Error!SubType {
        switch (self.peekToken().tag) {
            .identifier => {
                return .{ .name = try self.parseName() };
            },

            .keyword_fn => {
                _ = self.nextToken();

                if (!self.eatToken(.open_paren)) {
                    self.error_info = .{ .message = "expected a '('", .source_loc = self.tokenSourceLoc(self.peekToken()) };

                    return error.UnexpectedToken;
                }

                var parameter_subtypes: std.ArrayListUnmanaged(SubType) = .{};

                while (self.peekToken().tag != .eof and self.peekToken().tag != .close_paren) {
                    try parameter_subtypes.append(self.allocator, try self.parseSubType());

                    if (!self.eatToken(.comma) and self.peekToken().tag != .close_paren) {
                        self.error_info = .{ .message = "expected a ','", .source_loc = self.tokenSourceLoc(self.peekToken()) };

                        return error.UnexpectedToken;
                    }
                }

                if (!self.eatToken(.close_paren)) {
                    self.error_info = .{ .message = "expected a ')'", .source_loc = self.tokenSourceLoc(self.peekToken()) };

                    return error.UnexpectedToken;
                }

                const return_subtype = try self.parseSubType();

                const return_subtype_on_heap = try self.allocator.create(SubType);
                return_subtype_on_heap.* = return_subtype;

                return .{
                    .function = .{
                        .parameter_subtypes = try parameter_subtypes.toOwnedSlice(self.allocator),
                        .return_subtype = return_subtype_on_heap,
                    },
                };
            },

            .star => {
                _ = self.nextToken();

                const is_const = self.eatToken(.keyword_const);

                const child = try self.parseSubType();

                const child_on_heap = try self.allocator.create(SubType);
                child_on_heap.* = child;

                return .{
                    .pointer = .{
                        .size = .one,
                        .is_const = is_const,
                        .is_local = self.in_function,
                        .child_subtype = child_on_heap,
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

                const child = try self.parseSubType();

                const child_on_heap = try self.allocator.create(SubType);
                child_on_heap.* = child;

                return .{
                    .pointer = .{
                        .size = .many,
                        .is_const = is_const,
                        .is_local = self.in_function,
                        .child_subtype = child_on_heap,
                    },
                };
            },

            else => {},
        }

        self.error_info = .{ .message = "expected a type", .source_loc = self.tokenSourceLoc(self.peekToken()) };

        return error.InvalidType;
    }
};
