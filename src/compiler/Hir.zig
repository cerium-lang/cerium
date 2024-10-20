//! High Intermediate Representation.
//!
//! An unchecked stack-based intermediate representation lowered down from `Token`s.
//! Passed to `Sema` which checks all the instructions and types to be valid and lowers it down to `Lir`.
//! And then `Lir` gets lowered down to machine code.

const std = @import("std");

const Compilation = @import("Compilation.zig");
const Lexer = @import("Lexer.zig");
const Symbol = @import("Symbol.zig");
const Type = Symbol.Type;
const Token = @import("Token.zig");

const Hir = @This();

instructions: std.ArrayListUnmanaged(Instruction) = .{},

pub const SourceLoc = struct {
    line: u32 = 1,
    column: u32 = 1,
};

pub const Name = struct {
    buffer: []const u8,
    source_loc: SourceLoc,
};

/// A first concept of a type and should be resolved later by the semantic analyzer
pub const SubType = union(enum) {
    name: Name,
    function: Function,
    pointer: Pointer,
    @"struct": Struct,
    pure: Type,

    pub const Function = struct {
        parameter_subtypes: []const SubType,
        return_subtype: *const SubType,
    };

    pub const Pointer = struct {
        size: Type.Pointer.Size,
        is_const: bool,
        child_subtype: *const SubType,
    };

    pub const Struct = struct {
        subsymbols: []const SubSymbol,
    };
};

/// A first concept of a symbol and should be resolved later by the semantic analyzer
pub const SubSymbol = struct {
    name: Name,
    subtype: SubType,
    linkage: Symbol.Linkage,
};

pub const Instruction = union(enum) {
    /// Duplicate the top of the stack
    duplicate,
    /// Reverse the stack into nth depth
    reverse: u32,
    /// Pop the top of the stack
    pop,
    /// Push a string onto the stack
    string: []const u8,
    /// Push an integer onto the stack
    int: i128,
    /// Push a float onto the stack
    float: f64,
    /// Negate an integer or float on the top of the stack
    negate: SourceLoc,
    /// Reverse a boolean from true to false and from false to true
    bool_not: SourceLoc,
    /// Perform bitwise NOT operation on the bits of rhs (Which is to reverse its bits representation)
    bit_not: SourceLoc,
    /// Perform bitwise AND operation on the bits of lhs and rhs
    bit_and: SourceLoc,
    /// Perform bitwise OR operation on the bits of lhs and rhs
    bit_or: SourceLoc,
    /// Perform bitwise XOR operation on the bits of lhs and rhs
    bit_xor: SourceLoc,
    /// Get a pointer of a value on the stack
    reference: SourceLoc,
    /// Override the data that the pointer is pointing to
    write: SourceLoc,
    /// Read the data that the pointer is pointing to
    read: SourceLoc,
    /// Calculate the pointer of an element in an "size many" pointer
    get_element_ptr: SourceLoc,
    /// Calculate the pointer of a field in a struct (convert to struct pointer if needed)
    get_field_ptr: Name,
    /// Add two integers or floats or pointers on the top of the stack
    add: SourceLoc,
    /// Subtract two integers or floats or pointers on the top of the stack
    sub: SourceLoc,
    /// Multiply two integers or floats on the top of the stack
    mul: SourceLoc,
    /// Divide two integers or floats on the top of the stack
    div: SourceLoc,
    /// Compare between two integers or floats on the stack and check for order (in this case, lhs less than rhs)
    lt: SourceLoc,
    /// Compare between two integers or floats on the stack and check for order (in this case, lhs greater than rhs)
    gt: SourceLoc,
    /// Compare between two values on the stack and check for equality
    eql: SourceLoc,
    /// Shift to left the bits of lhs using rhs offset
    shl: SourceLoc,
    /// Shift to right the bits of lhs using rhs offset
    shr: SourceLoc,
    /// Cast a value to a different type
    cast: Cast,
    /// Place a machine-specific assembly in the output
    assembly: Assembly,
    /// Call a function pointer on the stack
    call: Call,
    /// Declare a function
    function: SubSymbol,
    /// Declare function parameters
    parameters: []const SubSymbol,
    /// Declare a constant that is replaced at compile time and acts as a placeholder for a value
    constant: SubSymbol,
    /// Same as `constant` but the type is unknown at the point of declaration
    constant_infer: SubSymbol,
    /// Declare a variable that is only known at runtime and doesn't get replaced by the compiler
    variable: SubSymbol,
    /// Same as `variable` but the type is unknown at the point of declaration
    variable_infer: SubSymbol,
    /// Same as `variable` but the variable is external
    external: SubSymbol,
    /// Declare a type alias
    type_alias: SubSymbol,
    /// Set a variable with a value on top of the stack
    set: Name,
    /// Get a value of a variable
    get: Name,
    /// Make a new block out of instructions
    block: Block,
    /// Unconditionally branch to a block
    br: Br,
    /// Conditionally branch to a block, condition is on the stack
    cond_br: CondBr,
    /// Start a new scope
    start_scope,
    /// End a scope
    end_scope,
    /// Return out of the function with a value on the stack
    ret: SourceLoc,
    /// Return out of the function without a value
    ret_void: SourceLoc,

    pub const Cast = struct {
        to: SubType,
        source_loc: SourceLoc,
    };

    pub const Assembly = struct {
        content: []const u8,
        output_constraint: ?OutputConstraint,
        input_constraints: []const []const u8,
        clobbers: []const []const u8,
        source_loc: SourceLoc,

        pub const OutputConstraint = struct {
            register: []const u8,
            subtype: SubType,
        };
    };

    pub const Call = struct {
        arguments_count: usize,
        source_loc: SourceLoc,
    };

    pub const Block = struct {
        id: u32,
    };

    pub const Br = struct {
        id: u32,
    };

    pub const CondBr = struct {
        true_id: u32,
        false_id: u32,
        source_loc: SourceLoc,
    };
};

pub const Parser = struct {
    allocator: std.mem.Allocator,

    env: Compilation.Environment,

    buffer: [:0]const u8,

    tokens: std.MultiArrayList(Token).Slice,
    token_index: u32,

    hir: Hir,

    block_id: ?u32 = null,

    error_info: ?ErrorInfo = null,

    pub const ErrorInfo = struct {
        message: []const u8,
        source_loc: SourceLoc,
    };

    pub const Error = error{
        InvalidType,
        InvalidString,
        InvalidChar,
        InvalidNumber,
        Redeclared,
        UnexpectedToken,
        UnexpectedStatement,
        UnexpectedExpression,
    } || std.mem.Allocator.Error;

    pub fn init(allocator: std.mem.Allocator, env: Compilation.Environment, buffer: [:0]const u8) std.mem.Allocator.Error!Parser {
        var tokens: std.MultiArrayList(Token) = .{};

        // Tokens should have a lowering rate of 2 to 1, we use that estimate to avoid reallocation
        try tokens.ensureTotalCapacity(allocator, buffer.len / 2);

        var lexer = Lexer.init(buffer);

        while (true) {
            const token = lexer.next();

            try tokens.append(allocator, token);

            if (token.tag == .eof) break;
        }

        var hir: Hir = .{};

        // Hir instructions are always less than or equal to the tokens length
        try hir.instructions.ensureTotalCapacity(allocator, tokens.len);

        return Parser{
            .allocator = allocator,
            .env = env,
            .buffer = buffer,
            .tokens = tokens.toOwnedSlice(),
            .token_index = 0,
            .hir = hir,
        };
    }

    pub fn deinit(self: *Parser) void {
        self.tokens.deinit(self.allocator);
    }

    fn nextToken(self: *Parser) Token {
        self.token_index += 1;
        return self.tokens.get(self.token_index - 1);
    }

    pub fn peekToken(self: Parser) Token {
        return self.tokens.get(self.token_index);
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
        return self.buffer[token.range.start..token.range.end];
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

            if (i == token.range.start) break;
        }

        return source_loc;
    }

    pub fn parse(self: *Parser) Error!void {
        while (self.peekToken().tag != .eof) {
            try self.parseTopLevelDeclaration();
        }
    }

    fn parseTopLevelDeclaration(self: *Parser) Error!void {
        switch (self.peekToken().tag) {
            .keyword_extern => try self.parseExternalDeclaration(),

            .keyword_fn => return self.parseFunctionDeclaration(.global),

            .keyword_const, .keyword_var => try self.parseVariableDeclaration(.global),

            .keyword_type => try self.parseTypeAlias(.global),

            else => {
                self.error_info = .{ .message = "expected a top level declaration", .source_loc = self.tokenSourceLoc(self.peekToken()) };

                return error.UnexpectedToken;
            },
        }

        if (!self.eatToken(.semicolon)) {
            self.error_info = .{ .message = "expected a ';'", .source_loc = self.tokenSourceLoc(self.peekToken()) };

            return error.UnexpectedToken;
        }
    }

    fn parseStmt(self: *Parser) Error!void {
        switch (self.peekToken().tag) {
            .keyword_const, .keyword_var => try self.parseVariableDeclaration(.local),

            .keyword_type => try self.parseTypeAlias(.local),

            .keyword_if => return self.parseConditional(),

            .keyword_while => return self.parseWhileLoop(),

            .keyword_break => try self.parseBreak(),

            .keyword_continue => try self.parseContinue(),

            .keyword_return => try self.parseReturn(),

            else => {
                try self.parseExpr(.lowest);

                try self.hir.instructions.append(self.allocator, .pop);
            },
        }

        if (!self.eatToken(.semicolon)) {
            self.error_info = .{ .message = "expected a ';'", .source_loc = self.tokenSourceLoc(self.peekToken()) };

            return error.UnexpectedToken;
        }
    }

    fn parseExternalDeclaration(self: *Parser) Error!void {
        const source_loc = self.tokenSourceLoc(self.nextToken());

        if (self.peekToken().tag == .keyword_fn) {
            try self.parseFunctionDeclaration(.external);
        } else if (self.peekToken().tag == .keyword_var) {
            try self.parseVariableDeclaration(.external);
        } else if (self.peekToken().tag == .keyword_const) {
            self.error_info = .{ .message = "'const' is declaring a compile time constant and cannot be used with 'extern'", .source_loc = self.tokenSourceLoc(self.peekToken()) };

            return error.UnexpectedToken;
        } else {
            self.error_info = .{ .message = "expected a function or variable declaration", .source_loc = source_loc };

            return error.UnexpectedToken;
        }
    }

    fn parseFunctionDeclaration(self: *Parser, linkage: Symbol.Linkage) Error!void {
        const source_loc = self.tokenSourceLoc(self.nextToken());

        const name = try self.parseName();

        const parameter_subsymbols = try self.parseFunctionParameters();

        const parameter_subtypes = try self.allocator.alloc(SubType, parameter_subsymbols.len);

        for (parameter_subsymbols, 0..) |parameter_subsymbol, i| {
            parameter_subtypes[i] = parameter_subsymbol.subtype;
        }

        const return_subtype = if (self.peekToken().tag == .open_brace or (self.peekToken().tag == .semicolon and linkage == .external))
            SubType{ .pure = .void }
        else
            try self.parseSubType();

        const return_subtype_on_heap = try self.allocator.create(SubType);
        return_subtype_on_heap.* = return_subtype;

        const function_subtype: SubType = .{
            .function = .{
                .parameter_subtypes = parameter_subtypes,
                .return_subtype = return_subtype_on_heap,
            },
        };

        const function_subtype_on_heap = try self.allocator.create(SubType);
        function_subtype_on_heap.* = function_subtype;

        const function_pointer_subtype: SubType = .{
            .pointer = .{
                .size = .one,
                .is_const = true,
                .child_subtype = function_subtype_on_heap,
            },
        };

        if (linkage == .external) {
            return self.hir.instructions.append(self.allocator, .{
                .external = .{
                    .name = name,
                    .subtype = function_pointer_subtype,
                    .linkage = linkage,
                },
            });
        }

        try self.hir.instructions.append(self.allocator, .{
            .function = .{
                .name = name,
                .subtype = function_pointer_subtype,
                .linkage = linkage,
            },
        });

        try self.hir.instructions.append(self.allocator, .{ .block = .{ .id = 0 } });

        self.block_id = 1;
        defer self.block_id = null;

        try self.hir.instructions.append(self.allocator, .start_scope);

        if (parameter_subsymbols.len != 0) {
            try self.hir.instructions.append(self.allocator, .{ .parameters = parameter_subsymbols });
        }

        try self.parseBody();

        if (self.hir.instructions.items.len == 0 or
            (self.hir.instructions.getLast() != .ret and
            self.hir.instructions.getLast() != .ret_void))
        {
            try self.hir.instructions.append(self.allocator, .{ .ret_void = source_loc });
        }

        try self.hir.instructions.append(self.allocator, .end_scope);
    }

    fn parseFunctionParameters(self: *Parser) Error![]SubSymbol {
        if (!self.eatToken(.open_paren)) {
            self.error_info = .{ .message = "expected a '('", .source_loc = self.tokenSourceLoc(self.peekToken()) };

            return error.UnexpectedToken;
        }

        var paramters: std.ArrayListUnmanaged(SubSymbol) = .{};

        while (!self.eatToken(.close_paren)) {
            if (self.peekToken().tag == .eof) {
                self.error_info = .{ .message = "expected a ')'", .source_loc = self.tokenSourceLoc(self.peekToken()) };

                return error.UnexpectedToken;
            }

            try paramters.append(self.allocator, .{
                .name = try self.parseName(),
                .subtype = try self.parseSubType(),
                .linkage = .local,
            });

            if (!self.eatToken(.comma) and self.peekToken().tag != .close_paren) {
                self.error_info = .{ .message = "expected a ',' after parameter", .source_loc = self.tokenSourceLoc(self.peekToken()) };

                return error.UnexpectedToken;
            }
        }

        return paramters.toOwnedSlice(self.allocator);
    }

    fn parseVariableDeclaration(self: *Parser, linkage: Symbol.Linkage) Error!void {
        const init_token = self.nextToken();

        const is_const = init_token.tag == .keyword_const;

        const name = try self.parseName();

        const maybe_subtype = if (self.peekToken().tag == .equal_sign)
            null
        else
            try self.parseSubType();

        if (maybe_subtype != null and linkage != .external and !self.eatToken(.equal_sign)) {
            self.error_info = .{ .message = "expected a '='", .source_loc = self.tokenSourceLoc(self.peekToken()) };

            return error.UnexpectedToken;
        } else if (self.eatToken(.equal_sign) and linkage == .external) {
            self.error_info = .{ .message = "'extern' variables cannot have an initializer", .source_loc = self.tokenSourceLoc(self.peekToken()) };

            return error.UnexpectedToken;
        }

        if (linkage == .external) {
            return self.hir.instructions.append(
                self.allocator,
                .{
                    .external = .{
                        .name = name,
                        .subtype = maybe_subtype.?,
                        .linkage = .global,
                    },
                },
            );
        }

        try self.parseExpr(.lowest);

        if (maybe_subtype) |subtype| {
            try self.hir.instructions.append(
                self.allocator,
                if (is_const)
                    .{
                        .constant = .{
                            .name = name,
                            .subtype = subtype,
                            .linkage = linkage,
                        },
                    }
                else
                    .{
                        .variable = .{
                            .name = name,
                            .subtype = subtype,
                            .linkage = linkage,
                        },
                    },
            );
        } else {
            try self.hir.instructions.append(
                self.allocator,
                if (is_const)
                    .{
                        .constant_infer = .{
                            .name = name,
                            .subtype = .{ .pure = .void },
                            .linkage = linkage,
                        },
                    }
                else
                    .{
                        .variable_infer = .{
                            .name = name,
                            .subtype = .{ .pure = .void },
                            .linkage = linkage,
                        },
                    },
            );
        }

        try self.hir.instructions.append(self.allocator, .{ .set = name });
    }

    fn parseTypeAlias(self: *Parser, linkage: Symbol.Linkage) Error!void {
        _ = self.nextToken();

        const name = try self.parseName();

        if (!self.eatToken(.equal_sign)) {
            self.error_info = .{ .message = "expected a '='", .source_loc = self.tokenSourceLoc(self.peekToken()) };

            return error.UnexpectedToken;
        }

        const subtype = try self.parseSubType();

        try self.hir.instructions.append(
            self.allocator,
            .{
                .type_alias = .{
                    .name = name,
                    .subtype = subtype,
                    .linkage = linkage,
                },
            },
        );
    }

    fn parseConditional(self: *Parser) Error!void {
        const end_block_id = self.block_id.?;
        self.block_id.? += 1;

        while (self.peekToken().tag == .keyword_if) {
            _ = self.nextToken();

            const condition_source_loc = self.tokenSourceLoc(self.peekToken());

            try self.parseExpr(.lowest);

            try self.hir.instructions.append(self.allocator, .{
                .cond_br = .{
                    .true_id = self.block_id.?,
                    .false_id = self.block_id.? + 1,
                    .source_loc = condition_source_loc,
                },
            });

            try self.hir.instructions.append(self.allocator, .{ .block = .{ .id = self.block_id.? } });
            self.block_id.? += 1;

            try self.hir.instructions.append(self.allocator, .start_scope);

            try self.parseBody();

            try self.hir.instructions.append(self.allocator, .end_scope);

            try self.hir.instructions.append(self.allocator, .{ .br = .{ .id = end_block_id } });

            if (self.eatToken(.keyword_else)) {
                if (self.peekToken().tag == .keyword_if) continue;

                try self.hir.instructions.append(self.allocator, .{ .block = .{ .id = self.block_id.? } });
                self.block_id.? += 1;

                try self.hir.instructions.append(self.allocator, .start_scope);

                try self.parseBody();

                try self.hir.instructions.append(self.allocator, .end_scope);

                try self.hir.instructions.append(self.allocator, .{ .br = .{ .id = end_block_id } });
            } else {
                try self.hir.instructions.append(self.allocator, .{ .block = .{ .id = self.block_id.? } });
                self.block_id.? += 1;

                try self.hir.instructions.append(self.allocator, .{ .br = .{ .id = end_block_id } });
            }

            break;
        }

        try self.hir.instructions.append(self.allocator, .{ .block = .{ .id = end_block_id } });
    }

    var maybe_header_block_id: ?u32 = null;
    var maybe_end_block_id: ?u32 = null;

    fn parseWhileLoop(self: *Parser) Error!void {
        _ = self.nextToken();

        const header_block_id = self.block_id.?;
        self.block_id.? += 1;

        const previous_header_block_id = maybe_header_block_id;
        maybe_header_block_id = header_block_id;
        defer maybe_header_block_id = previous_header_block_id;

        const body_block_id = self.block_id.?;
        self.block_id.? += 1;

        const end_block_id = self.block_id.?;
        self.block_id.? += 1;

        const previous_end_block_id = maybe_end_block_id;
        maybe_end_block_id = end_block_id;
        defer maybe_end_block_id = previous_end_block_id;

        try self.hir.instructions.append(self.allocator, .{ .br = .{ .id = header_block_id } });

        try self.hir.instructions.append(self.allocator, .{ .block = .{ .id = header_block_id } });

        const condition_source_loc = self.tokenSourceLoc(self.peekToken());

        try self.parseExpr(.lowest);

        try self.hir.instructions.append(self.allocator, .{ .cond_br = .{
            .true_id = body_block_id,
            .false_id = end_block_id,
            .source_loc = condition_source_loc,
        } });

        try self.hir.instructions.append(self.allocator, .{ .block = .{ .id = body_block_id } });

        try self.hir.instructions.append(self.allocator, .start_scope);

        try self.parseBody();

        try self.hir.instructions.append(self.allocator, .end_scope);

        try self.hir.instructions.append(self.allocator, .{ .br = .{ .id = header_block_id } });

        try self.hir.instructions.append(self.allocator, .{ .block = .{ .id = end_block_id } });
    }

    fn parseContinue(self: *Parser) Error!void {
        const source_loc = self.tokenSourceLoc(self.nextToken());

        if (maybe_header_block_id) |header_block_id| {
            return self.hir.instructions.append(self.allocator, .{ .br = .{ .id = header_block_id } });
        }

        self.error_info = .{ .message = "expected the continue statement to be inside a loop", .source_loc = source_loc };

        return error.UnexpectedStatement;
    }

    fn parseBreak(self: *Parser) Error!void {
        const source_loc = self.tokenSourceLoc(self.nextToken());

        if (maybe_end_block_id) |end_block_id| {
            return self.hir.instructions.append(self.allocator, .{ .br = .{ .id = end_block_id } });
        }

        self.error_info = .{ .message = "expected the break statement to be inside a loop", .source_loc = source_loc };

        return error.UnexpectedStatement;
    }

    fn parseReturn(self: *Parser) Error!void {
        const source_loc = self.tokenSourceLoc(self.nextToken());

        if (self.peekToken().tag == .semicolon) {
            try self.hir.instructions.append(self.allocator, .{ .ret_void = source_loc });
        } else {
            try self.parseExpr(.lowest);

            try self.hir.instructions.append(self.allocator, .{ .ret = source_loc });
        }
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
        cast,
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
                .keyword_as => .cast,
                .open_paren => .call,
                .open_bracket => .subscript,
                .period => .subscript,

                else => .lowest,
            };
        }
    };

    fn parseExpr(self: *Parser, precedence: Precedence) Error!void {
        try self.parseUnaryExpr();

        while (@intFromEnum(Precedence.from(self.peekToken())) > @intFromEnum(precedence) and self.peekToken().tag != .semicolon) {
            try self.parseBinaryExpr();
        }
    }

    fn parseUnaryExpr(self: *Parser) Error!void {
        switch (self.peekToken().tag) {
            .identifier => try self.parseIdentifier(),

            .string_literal => try self.parseString(),

            .char_literal => try self.parseChar(),

            .int => try self.parseInt(),

            .float => try self.parseFloat(),

            .keyword_asm => try self.parseAssembly(),

            .open_paren => try self.parseParentheses(),

            .minus, .bang, .tilde, .ampersand => try self.parseUnaryOperation(),

            else => {
                self.error_info = .{ .message = "expected a statement or an expression", .source_loc = self.tokenSourceLoc(self.peekToken()) };

                return error.UnexpectedToken;
            },
        }
    }

    fn parseIdentifier(self: *Parser) Error!void {
        try self.hir.instructions.append(self.allocator, .{ .get = try self.parseName() });
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

    fn parseString(self: *Parser) Error!void {
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

        try self.hir.instructions.append(self.allocator, .{ .string = unescaped });
    }

    fn parseChar(self: *Parser) Error!void {
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

        try self.hir.instructions.append(self.allocator, .{ .int = decoded });
    }

    fn parseInt(self: *Parser) Error!void {
        const value = std.fmt.parseInt(i128, self.tokenValue(self.nextToken()), 0) catch {
            self.error_info = .{ .message = "invalid number", .source_loc = self.tokenSourceLoc(self.peekToken()) };

            return error.InvalidNumber;
        };

        try self.hir.instructions.append(self.allocator, .{ .int = value });
    }

    fn parseFloat(self: *Parser) Error!void {
        const value = std.fmt.parseFloat(f64, self.tokenValue(self.nextToken())) catch {
            self.error_info = .{ .message = "invalid number", .source_loc = self.tokenSourceLoc(self.peekToken()) };

            return error.InvalidNumber;
        };

        try self.hir.instructions.append(self.allocator, .{ .float = value });
    }

    fn parseAssembly(self: *Parser) Error!void {
        const asm_keyword_token = self.nextToken();

        var content: std.ArrayListUnmanaged(u8) = .{};

        if (!self.eatToken(.open_brace)) {
            self.error_info = .{ .message = "expected a '{'", .source_loc = self.tokenSourceLoc(self.peekToken()) };

            return error.UnexpectedToken;
        }

        var input_constraints: []const []const u8 = &.{};
        var output_constraint: ?Instruction.Assembly.OutputConstraint = null;
        var clobbers: []const []const u8 = &.{};

        while (!self.eatToken(.close_brace)) {
            if (self.peekToken().tag != .string_literal) {
                self.error_info = .{ .message = "expected a valid string", .source_loc = self.tokenSourceLoc(self.peekToken()) };

                return error.UnexpectedToken;
            }

            try self.parseString();

            try content.appendSlice(self.allocator, self.hir.instructions.pop().string);

            if (self.eatToken(.colon)) {
                if (self.peekToken().tag != .colon) {
                    output_constraint = try self.parseAssemblyOutputConstraint();
                }
            }

            if (self.eatToken(.colon)) {
                if (self.peekToken().tag != .colon) {
                    input_constraints = try self.parseAssemblyInputConstraints();
                }
            }

            if (self.eatToken(.colon)) {
                if (self.peekToken().tag != .close_brace) {
                    clobbers = try self.parseAssemblyClobbers();
                }
            }

            try content.append(self.allocator, '\n');

            if (self.peekToken().tag != .close_brace and (self.peekToken().tag != .colon or self.peekToken().tag != .string_literal)) {
                self.error_info = .{ .message = "expected a '}'", .source_loc = self.tokenSourceLoc(self.peekToken()) };

                return error.UnexpectedToken;
            }
        }

        try self.hir.instructions.append(self.allocator, .{
            .assembly = .{
                .content = try content.toOwnedSlice(self.allocator),
                .input_constraints = input_constraints,
                .output_constraint = output_constraint,
                .clobbers = clobbers,
                .source_loc = self.tokenSourceLoc(asm_keyword_token),
            },
        });
    }

    fn parseAssemblyInputConstraints(self: *Parser) Error![][]const u8 {
        var constraints = std.ArrayList([]const u8).init(self.allocator);

        while (self.peekToken().tag != .eof and self.peekToken().tag != .colon and self.peekToken().tag != .close_brace) {
            try constraints.append(try self.parseAssemblyInputConstraint());

            if (!self.eatToken(.comma) and self.peekToken().tag != .colon and self.peekToken().tag != .close_brace) {
                self.error_info = .{ .message = "expected a ','", .source_loc = self.tokenSourceLoc(self.peekToken()) };

                return error.UnexpectedToken;
            }
        }

        try self.hir.instructions.append(self.allocator, .{ .reverse = @intCast(constraints.items.len) });

        return constraints.toOwnedSlice();
    }

    fn parseAssemblyInputConstraint(self: *Parser) Error![]const u8 {
        if (self.peekToken().tag != .string_literal) {
            self.error_info = .{ .message = "expected a valid string", .source_loc = self.tokenSourceLoc(self.peekToken()) };

            return error.UnexpectedToken;
        }

        try self.parseString();

        const register = self.hir.instructions.pop().string;

        if (!self.eatToken(.open_paren)) {
            self.error_info = .{ .message = "expected a '('", .source_loc = self.tokenSourceLoc(self.peekToken()) };

            return error.UnexpectedToken;
        }

        try self.parseExpr(.lowest);

        if (!self.eatToken(.close_paren)) {
            self.error_info = .{ .message = "expected a ')'", .source_loc = self.tokenSourceLoc(self.peekToken()) };

            return error.UnexpectedToken;
        }

        return register;
    }

    fn parseAssemblyOutputConstraint(self: *Parser) Error!Instruction.Assembly.OutputConstraint {
        if (self.peekToken().tag != .string_literal) {
            self.error_info = .{ .message = "expected a valid string", .source_loc = self.tokenSourceLoc(self.peekToken()) };

            return error.UnexpectedToken;
        }

        try self.parseString();

        const register = self.hir.instructions.pop().string;

        if (!self.eatToken(.open_paren)) {
            self.error_info = .{ .message = "expected a '('", .source_loc = self.tokenSourceLoc(self.peekToken()) };

            return error.UnexpectedToken;
        }

        const subtype = try self.parseSubType();

        if (!self.eatToken(.close_paren)) {
            self.error_info = .{ .message = "expected a ')'", .source_loc = self.tokenSourceLoc(self.peekToken()) };

            return error.UnexpectedToken;
        }

        return Instruction.Assembly.OutputConstraint{
            .register = register,
            .subtype = subtype,
        };
    }

    fn parseAssemblyClobbers(self: *Parser) Error![][]const u8 {
        var clobbers = std.ArrayList([]const u8).init(self.allocator);

        while (self.peekToken().tag != .eof and self.peekToken().tag != .close_brace) {
            if (self.peekToken().tag != .string_literal) {
                self.error_info = .{ .message = "expected a valid string", .source_loc = self.tokenSourceLoc(self.peekToken()) };

                return error.UnexpectedToken;
            }

            try self.parseString();

            try clobbers.append(self.hir.instructions.pop().string);

            if (!self.eatToken(.comma) and self.peekToken().tag != .close_brace) {
                self.error_info = .{ .message = "expected a ','", .source_loc = self.tokenSourceLoc(self.peekToken()) };

                return error.UnexpectedToken;
            }
        }

        return clobbers.toOwnedSlice();
    }

    fn parseParentheses(self: *Parser) Error!void {
        _ = self.nextToken();

        try self.parseExpr(.lowest);

        if (!self.eatToken(.close_paren)) {
            self.error_info = .{ .message = "expected a ')'", .source_loc = self.tokenSourceLoc(self.peekToken()) };

            return error.UnexpectedToken;
        }
    }

    fn parseUnaryOperation(self: *Parser) Error!void {
        const operator_token = self.nextToken();
        const source_loc = self.tokenSourceLoc(operator_token);

        try self.parseExpr(.prefix);

        switch (operator_token.tag) {
            .minus => {
                try self.hir.instructions.append(self.allocator, .{ .negate = source_loc });
            },

            .bang => {
                try self.hir.instructions.append(self.allocator, .{ .bool_not = source_loc });
            },

            .tilde => {
                try self.hir.instructions.append(self.allocator, .{ .bit_not = source_loc });
            },

            .ampersand => {
                if (self.hir.instructions.items[self.hir.instructions.items.len - 1] == .read and
                    (self.hir.instructions.items[self.hir.instructions.items.len - 2] == .get_element_ptr or
                    self.hir.instructions.items[self.hir.instructions.items.len - 2] == .get_field_ptr))
                {
                    _ = self.hir.instructions.pop();
                } else {
                    try self.hir.instructions.append(self.allocator, .{ .reference = source_loc });
                }
            },

            else => unreachable,
        }
    }

    fn parseBinaryExpr(self: *Parser) Error!void {
        switch (self.peekToken().tag) {
            .plus,
            .minus,
            .star,
            .forward_slash,
            .equal_sign,
            .less_than,
            .greater_than,
            .double_less_than,
            .double_greater_than,
            .ampersand,
            .pipe,
            .caret,
            .double_equal_sign,
            .bang_equal_sign,
            => try self.parseBinaryOperation(),

            .keyword_as => try self.parseCast(),

            .open_paren => try self.parseCall(),

            .open_bracket => try self.parseSubscript(),

            .period => try self.parseFieldAccess(),

            else => {
                self.error_info = .{ .message = "expected a statement or an expression", .source_loc = self.tokenSourceLoc(self.peekToken()) };

                return error.UnexpectedToken;
            },
        }
    }

    fn parseBinaryOperation(self: *Parser) Error!void {
        const operator_token = self.nextToken();
        const source_loc = self.tokenSourceLoc(operator_token);

        if (operator_token.tag != .equal_sign) {
            try self.parseExpr(.lowest);
        }

        switch (operator_token.tag) {
            .plus => {
                try self.hir.instructions.append(self.allocator, .{ .add = source_loc });
            },

            .minus => {
                try self.hir.instructions.append(self.allocator, .{ .sub = source_loc });
            },

            .star => {
                try self.hir.instructions.append(self.allocator, .{ .mul = source_loc });
            },

            .forward_slash => {
                try self.hir.instructions.append(self.allocator, .{ .div = source_loc });
            },

            .equal_sign => {
                const last_instruction = self.hir.instructions.pop();

                try self.parseExpr(.lowest);

                if (last_instruction == .read) {
                    // 1:
                    // lhs
                    // rhs
                    //
                    // 2:
                    // lhs
                    // rhs
                    // rhs
                    //
                    // 3:
                    // rhs
                    // rhs
                    // lhs

                    try self.hir.instructions.append(self.allocator, .duplicate);
                    try self.hir.instructions.append(self.allocator, .{ .reverse = 3 });
                    try self.hir.instructions.append(self.allocator, .{ .write = source_loc });
                } else if (last_instruction == .get) {
                    try self.hir.instructions.append(self.allocator, .duplicate);
                    try self.hir.instructions.append(self.allocator, .{ .set = last_instruction.get });
                } else {
                    self.error_info = .{ .message = "expected an identifier or a pointer dereference", .source_loc = source_loc };

                    return error.UnexpectedExpression;
                }
            },

            .less_than => {
                try self.hir.instructions.append(self.allocator, .{ .lt = source_loc });
            },

            .greater_than => {
                try self.hir.instructions.append(self.allocator, .{ .gt = source_loc });
            },

            .double_less_than => {
                try self.hir.instructions.append(self.allocator, .{ .shl = source_loc });
            },

            .double_greater_than => {
                try self.hir.instructions.append(self.allocator, .{ .shr = source_loc });
            },

            .ampersand => {
                try self.hir.instructions.append(self.allocator, .{ .bit_and = source_loc });
            },

            .pipe => {
                try self.hir.instructions.append(self.allocator, .{ .bit_or = source_loc });
            },

            .caret => {
                try self.hir.instructions.append(self.allocator, .{ .bit_xor = source_loc });
            },

            .double_equal_sign, .bang_equal_sign => {
                try self.hir.instructions.append(self.allocator, .{ .eql = source_loc });

                if (operator_token.tag == .bang_equal_sign) {
                    try self.hir.instructions.append(self.allocator, .{ .bool_not = source_loc });
                }
            },

            else => unreachable,
        }
    }

    fn parseCast(self: *Parser) Error!void {
        const source_loc = self.tokenSourceLoc(self.nextToken());

        const to = try self.parseSubType();

        try self.hir.instructions.append(self.allocator, .{ .cast = .{ .to = to, .source_loc = source_loc } });
    }

    fn parseCall(self: *Parser) Error!void {
        const source_loc = self.tokenSourceLoc(self.nextToken());

        const arguments_count = try self.parseCallArguments();

        try self.hir.instructions.append(self.allocator, .{ .reverse = arguments_count + 1 });

        try self.hir.instructions.append(self.allocator, .{
            .call = .{
                .arguments_count = arguments_count,
                .source_loc = source_loc,
            },
        });
    }

    fn parseCallArguments(self: *Parser) Error!u32 {
        var count: u32 = 0;

        while (self.peekToken().tag != .eof and self.peekToken().tag != .close_paren) {
            try self.parseExpr(.lowest);
            count += 1;

            if (!self.eatToken(.comma) and self.peekToken().tag != .close_paren) {
                self.error_info = .{ .message = "expected a ','", .source_loc = self.tokenSourceLoc(self.peekToken()) };

                return error.UnexpectedToken;
            }
        }

        if (!self.eatToken(.close_paren)) {
            self.error_info = .{ .message = "expected a ')'", .source_loc = self.tokenSourceLoc(self.peekToken()) };

            return error.UnexpectedToken;
        }

        return count;
    }

    fn parseSubscript(self: *Parser) Error!void {
        const source_loc = self.tokenSourceLoc(self.nextToken());

        try self.parseExpr(.subscript);

        if (!self.eatToken(.close_bracket)) {
            self.error_info = .{ .message = "expected a ']'", .source_loc = self.tokenSourceLoc(self.peekToken()) };

            return error.UnexpectedToken;
        }

        try self.hir.instructions.append(self.allocator, .{ .get_element_ptr = source_loc });
        try self.hir.instructions.append(self.allocator, .{ .read = source_loc });
    }

    fn parseFieldAccess(self: *Parser) Error!void {
        _ = self.nextToken();

        if (self.peekToken().tag == .star) {
            try self.hir.instructions.append(self.allocator, .{ .read = self.tokenSourceLoc(self.nextToken()) });
        } else {
            const name = try self.parseName();

            if (self.hir.instructions.items[self.hir.instructions.items.len - 1] == .read and
                self.hir.instructions.items[self.hir.instructions.items.len - 2] == .get_field_ptr)
            {
                _ = self.hir.instructions.pop();
            }

            try self.hir.instructions.append(self.allocator, .{ .get_field_ptr = name });
            try self.hir.instructions.append(self.allocator, .{ .read = name.source_loc });
        }
    }

    fn parseName(self: *Parser) Error!Name {
        if (self.peekToken().tag != .identifier) {
            self.error_info = .{ .message = "expected a name", .source_loc = self.tokenSourceLoc(self.peekToken()) };

            return error.UnexpectedToken;
        }

        const token = self.nextToken();

        return Name{ .buffer = self.tokenValue(token), .source_loc = self.tokenSourceLoc(token) };
    }

    fn parseSubType(self: *Parser) Error!SubType {
        switch (self.peekToken().tag) {
            .identifier => {
                return SubType{ .name = try self.parseName() };
            },

            .keyword_struct => {
                _ = self.nextToken();

                if (!self.eatToken(.open_brace)) {
                    self.error_info = .{ .message = "expected a '{'", .source_loc = self.tokenSourceLoc(self.peekToken()) };

                    return error.UnexpectedToken;
                }

                var subsymbols: std.ArrayListUnmanaged(SubSymbol) = .{};

                var fields_hashset = std.StringHashMapUnmanaged(void){};
                defer fields_hashset.deinit(self.allocator);

                while (self.peekToken().tag != .close_brace) {
                    const name = try self.parseName();

                    if (fields_hashset.get(name.buffer)) |_| {
                        var error_message_buf: std.ArrayListUnmanaged(u8) = .{};

                        try error_message_buf.writer(self.allocator).print("redeclaration of '{s}' in struct fields", .{name.buffer});

                        self.error_info = .{ .message = error_message_buf.items, .source_loc = name.source_loc };

                        return error.Redeclared;
                    }

                    try fields_hashset.put(self.allocator, name.buffer, {});

                    try subsymbols.append(self.allocator, .{
                        .name = name,
                        .subtype = try self.parseSubType(),
                        .linkage = .local,
                    });

                    if (!self.eatToken(.comma) and self.peekToken().tag != .close_brace) {
                        self.error_info = .{ .message = "expected a ','", .source_loc = self.tokenSourceLoc(self.peekToken()) };

                        return error.UnexpectedToken;
                    }
                }

                if (!self.eatToken(.close_brace)) {
                    self.error_info = .{ .message = "expected a '}'", .source_loc = self.tokenSourceLoc(self.peekToken()) };

                    return error.UnexpectedToken;
                }

                return SubType{ .@"struct" = .{ .subsymbols = try subsymbols.toOwnedSlice(self.allocator) } };
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

                return SubType{
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

                return SubType{
                    .pointer = .{
                        .size = .one,
                        .is_const = is_const,
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

                return SubType{
                    .pointer = .{
                        .size = .many,
                        .is_const = is_const,
                        .child_subtype = child_on_heap,
                    },
                };
            },

            else => {},
        }

        self.error_info = .{ .message = "expected a type", .source_loc = self.tokenSourceLoc(self.peekToken()) };

        return error.InvalidType;
    }

    fn parseBody(self: *Parser) Error!void {
        if (!self.eatToken(.open_brace)) {
            self.error_info = .{ .message = "expected a '{'", .source_loc = self.tokenSourceLoc(self.peekToken()) };

            return error.UnexpectedToken;
        }

        while (!self.eatToken(.close_brace)) {
            try self.parseStmt();

            if (self.peekToken().tag == .eof) {
                self.error_info = .{ .message = "expected a '}'", .source_loc = self.tokenSourceLoc(self.peekToken()) };

                return error.UnexpectedToken;
            }
        }
    }
};
