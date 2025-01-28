//! Syntax Intermediate Representation.
//!
//! An unchecked stack-based intermediate representation lowered down from `Token`s.
//! Passed to `Sema` which checks all the instructions and types to be valid and lowers it down to `Air`.
//! And then `Air` gets lowered down to machine code.

const std = @import("std");

const Compilation = @import("Compilation.zig");
const Lexer = @import("Lexer.zig");
const Symbol = @import("Symbol.zig");
const Scope = Symbol.Scope;
const Type = Symbol.Type;
const Token = @import("Token.zig");

const Sir = @This();

module_name: []const u8,

imports: std.ArrayListUnmanaged(Name) = .{},

global_assembly: std.ArrayListUnmanaged(u8) = .{},
external_declarations: std.ArrayListUnmanaged(SubSymbol) = .{},
type_aliases: std.StringArrayHashMapUnmanaged(SubSymbol) = .{},
global_constants: std.StringArrayHashMapUnmanaged(Definition) = .{},
global_variables: std.StringArrayHashMapUnmanaged(Definition) = .{},
functions: std.StringArrayHashMapUnmanaged(Definition) = .{},

pub const Definition = struct {
    exported: bool,
    subsymbol: SubSymbol,
    instructions: std.ArrayListUnmanaged(Instruction) = .{},
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
    negate: u32,
    /// Reverse a boolean from true to false and from false to true
    bool_not: u32,
    /// Perform bitwise NOT operation on the bits of rhs (Which is to reverse its bits representation)
    bit_not: u32,
    /// Perform bitwise AND operation on the bits of lhs and rhs
    bit_and: u32,
    /// Perform bitwise OR operation on the bits of lhs and rhs
    bit_or: u32,
    /// Perform bitwise XOR operation on the bits of lhs and rhs
    bit_xor: u32,
    /// Reference a value on the stack
    /// 1. If the value is a comptime value, it will be hoisted into a global variable and then the pointer of the global variable
    /// will be on the stack
    /// 2. If the value is a runtime value, it will be in a local variable and then the pointer of the local variable will be on the stack
    /// 3. If the previous instruction is a `read` air instruction, it will be removed and the pointer of the value will be on the stack
    reference,
    /// Override the data that the pointer is pointing to
    write: u32,
    /// Read the data that the pointer is pointing to
    read: u32,
    /// Add two integers or floats or pointers on the top of the stack
    add: u32,
    /// Subtract two integers or floats or pointers on the top of the stack
    sub: u32,
    /// Multiply two integers or floats on the top of the stack
    mul: u32,
    /// Divide two integers or floats on the top of the stack
    div: u32,
    /// Remainder of two integers or floats on the top of the stack
    rem: u32,
    /// Compare between two integers or floats on the stack and check for order (in this case, lhs less than rhs)
    lt: u32,
    /// Compare between two integers or floats on the stack and check for order (in this case, lhs greater than rhs)
    gt: u32,
    /// Compare between two values on the stack and check for equality
    eql: u32,
    /// Shift to left the bits of lhs using rhs offset
    shl: u32,
    /// Shift to right the bits of lhs using rhs offset
    shr: u32,
    /// Cast a value to a different type
    cast: Cast,
    /// Place a machine-specific inline assembly in the output
    inline_assembly: InlineAssembly,
    /// Call a function pointer on the stack
    call: Call,
    /// Declare function parameters
    parameters: []const SubSymbol,
    /// Define a constant that is replaced at compile time and acts as a placeholder for a value
    /// Pops the value from the stack and doesn't need `set` instruction after it unlike `variable`
    constant: SubSymbol,
    /// Declare a variable that is only known at runtime and doesn't get replaced by the compiler
    variable: SubSymbol,
    /// Same as `variable` but the type is unknown at the point of declaration
    variable_infer: SubSymbol,
    /// Set a variable with a value on top of the stack
    set: Name,
    /// Get a value of a variable
    get: Name,
    /// Should be used before parsing the index of element access (i.e array[index]) or slicing (i.e array[start..end])
    pre_get_element: u32,
    /// Get an element in a "size many" pointer
    get_element: u32,
    /// Get a field in a struct
    get_field: Name,
    /// Make a new slice out of a "size many" pointer
    make_slice: u32,
    /// Start a new block
    block: u32,
    /// Unconditionally branch to a block
    br: u32,
    /// Conditionally branch to a block, condition is on the stack
    cond_br: CondBr,
    /// Switch on value to branch to a block
    @"switch": Switch,
    /// Start a new scope
    start_scope,
    /// End a scope
    end_scope,
    /// Return out of the function with a value on the stack
    ret: u32,
    /// Return out of the function without a value
    ret_void: u32,

    pub const Cast = struct {
        to: SubType,
        token_start: u32,
    };

    pub const InlineAssembly = struct {
        content: []const u8,
        output_constraint: ?OutputConstraint,
        input_constraints: []const []const u8,
        clobbers: []const []const u8,
        token_start: u32,

        pub const OutputConstraint = struct {
            register: []const u8,
            subtype: SubType,
        };
    };

    pub const Call = struct {
        arguments_count: usize,
        token_start: u32,
    };

    pub const CondBr = struct {
        true_id: u32,
        false_id: u32,
        token_start: u32,
    };

    pub const Switch = struct {
        case_block_ids: []const u32,
        case_token_starts: []const u32,
        else_block_id: u32,
        token_start: u32,
    };
};

pub fn deinit(self: *Sir, allocator: std.mem.Allocator) void {
    for (self.global_variables.values()) |*global_variable| {
        global_variable.instructions.deinit(allocator);
    }

    for (self.functions.values()) |*function| {
        function.instructions.deinit(allocator);
    }

    self.global_assembly.deinit(allocator);
    self.external_declarations.deinit(allocator);
    self.global_constants.deinit(allocator);
    self.global_variables.deinit(allocator);
    self.type_aliases.deinit(allocator);
    self.functions.deinit(allocator);
}

pub const SourceLoc = struct {
    line: u32 = 1,
    column: u32 = 1,

    pub fn find(buffer: []const u8, start: u32) SourceLoc {
        var source_loc: SourceLoc = .{};

        var line_start: usize = 0;

        while (std.mem.indexOfScalarPos(u8, buffer, line_start, '\n')) |i| {
            if (i >= start) break;
            source_loc.line += 1;
            line_start = i + 1;
        }

        for (buffer[line_start..], line_start..) |c, i| {
            if (i >= start) break;
            source_loc.line += @intFromBool(c == '\n');
            source_loc.column += 1;
        }

        return source_loc;
    }
};

pub const Name = struct {
    buffer: []const u8,
    token_start: u32,
};

/// A first concept of a type and should be resolved later by the semantic analyzer
pub const SubType = union(enum) {
    name: Name,
    function: Function,
    pointer: Pointer,
    @"struct": Struct,
    @"enum": Enum,
    array: Array,
    pure: Type,

    pub const Function = struct {
        parameter_subtypes: []const SubType,
        is_var_args: bool,
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

    pub const Enum = struct {
        subtype: *const SubType,
        fields: []const Field,
        token_start: u32,

        pub const Field = struct {
            name: Name,
            value: i128,
        };
    };

    pub const Array = struct {
        len: usize,
        child_subtype: *const SubType,
    };
};

/// A first concept of a symbol and should be resolved later by the semantic analyzer
pub const SubSymbol = struct {
    name: Name,
    subtype: SubType,
};

pub const Parser = struct {
    allocator: std.mem.Allocator,

    env: Compilation.Environment,

    buffer: [:0]const u8,

    tokens: std.MultiArrayList(Token).Slice,
    token_index: u32,

    sir: Sir = .{ .module_name = undefined },

    sir_instructions: *std.ArrayListUnmanaged(Instruction) = undefined,

    block_id: u32 = 0,

    defer_stack: std.ArrayListUnmanaged([]Instruction) = .{},
    scope_defer_count: u32 = 0,

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
        UnhandledSwitchCases,
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

        return Parser{
            .allocator = allocator,
            .env = env,
            .buffer = buffer,
            .tokens = tokens.toOwnedSlice(),
            .token_index = 0,
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

    pub fn parse(self: *Parser) Error!void {
        if (!self.eatToken(.keyword_module)) {
            self.error_info = .{ .message = "expected 'module' keyword, unnamed modules are not allowed", .source_loc = SourceLoc.find(self.buffer, 0) };

            return error.UnexpectedToken;
        }

        self.sir.module_name = (try self.parseName()).buffer;

        if (!self.eatToken(.semicolon)) {
            self.error_info = .{ .message = "expected ';'", .source_loc = SourceLoc.find(self.buffer, 0) };

            return error.UnexpectedToken;
        }

        while (self.peekToken().tag != .eof) {
            try self.parseTopLevelStmt();
        }
    }

    fn parseTopLevelStmt(self: *Parser) Error!void {
        switch (self.peekToken().tag) {
            .keyword_extern => try self.parseExternalDeclaration(),

            .keyword_export => return self.parseExportDeclaration(),

            .keyword_import => try self.parseImport(),

            .keyword_asm => return self.parseGlobalAssembly(),

            .keyword_fn => return self.parseFunctionDeclaration(false, false),

            .keyword_const, .keyword_var => try self.parseVariableDeclaration(true, false, false),

            .keyword_type => try self.parseTypeAlias(),

            else => {
                self.error_info = .{ .message = "expected a top level statement", .source_loc = SourceLoc.find(self.buffer, self.peekToken().range.start) };

                return error.UnexpectedToken;
            },
        }

        if (!self.eatToken(.semicolon)) {
            self.error_info = .{ .message = "expected a ';'", .source_loc = SourceLoc.find(self.buffer, self.peekToken().range.start) };

            return error.UnexpectedToken;
        }
    }

    fn parseStmt(self: *Parser, expect_semicolon: bool) Error!void {
        switch (self.peekToken().tag) {
            .keyword_const, .keyword_var => try self.parseVariableDeclaration(false, false, false),

            .keyword_switch => return self.parseSwitch(),

            .keyword_if => return self.parseConditional(),

            .keyword_while => return self.parseWhileLoop(),

            .keyword_break => try self.parseBreak(),

            .keyword_continue => try self.parseContinue(),

            .keyword_defer => return self.parseDefer(),

            .keyword_return => try self.parseReturn(),

            .open_brace => {
                try self.sir_instructions.append(self.allocator, .start_scope);

                try self.parseBody();

                try self.sir_instructions.append(self.allocator, .end_scope);

                return;
            },

            else => {
                try self.parseExpr(.lowest);

                try self.sir_instructions.append(self.allocator, .pop);
            },
        }

        if (expect_semicolon and !self.eatToken(.semicolon)) {
            self.error_info = .{ .message = "expected a ';'", .source_loc = SourceLoc.find(self.buffer, self.peekToken().range.start) };

            return error.UnexpectedToken;
        }
    }

    fn parseExternalDeclaration(self: *Parser) Error!void {
        _ = self.nextToken();

        if (self.peekToken().tag == .keyword_fn) {
            try self.parseFunctionDeclaration(true, false);
        } else if (self.peekToken().tag == .keyword_var) {
            try self.parseVariableDeclaration(true, true, false);
        } else if (self.peekToken().tag == .keyword_const) {
            self.error_info = .{ .message = "'const' is declaring a compile time constant and cannot be used with 'extern'", .source_loc = SourceLoc.find(self.buffer, self.peekToken().range.start) };

            return error.UnexpectedToken;
        } else {
            self.error_info = .{ .message = "expected a function or variable declaration", .source_loc = SourceLoc.find(self.buffer, self.peekToken().range.start) };

            return error.UnexpectedToken;
        }
    }

    fn parseExportDeclaration(self: *Parser) Error!void {
        _ = self.nextToken();

        if (self.peekToken().tag == .keyword_fn) {
            try self.parseFunctionDeclaration(false, true);
        } else if (self.peekToken().tag == .keyword_var) {
            try self.parseVariableDeclaration(true, false, true);

            if (!self.eatToken(.semicolon)) {
                self.error_info = .{ .message = "expected a ';'", .source_loc = SourceLoc.find(self.buffer, self.peekToken().range.start) };

                return error.UnexpectedToken;
            }
        } else if (self.peekToken().tag == .keyword_const) {
            self.error_info = .{ .message = "'const' is declaring a compile time constant and cannot be used with 'export'", .source_loc = SourceLoc.find(self.buffer, self.peekToken().range.start) };

            return error.UnexpectedToken;
        } else {
            self.error_info = .{ .message = "expected a function or variable declaration", .source_loc = SourceLoc.find(self.buffer, self.peekToken().range.start) };

            return error.UnexpectedToken;
        }
    }

    fn parseImport(self: *Parser) Error!void {
        _ = self.nextToken();

        if (self.peekToken().tag != .string_literal) {
            self.error_info = .{ .message = "expected a valid string", .source_loc = SourceLoc.find(self.buffer, self.peekToken().range.start) };

            return error.UnexpectedToken;
        }

        const file_path: Name = .{ .token_start = self.peekToken().range.start, .buffer = self.tokenValue(self.nextToken()) };

        try self.sir.imports.append(self.allocator, file_path);
    }

    fn parseFunctionDeclaration(
        self: *Parser,
        comptime external: bool,
        comptime exported: bool,
    ) Error!void {
        const fn_keyword_start = self.nextToken().range.start;

        const name = try self.parseName();

        var is_var_args = false;

        const parameter_subsymbols = try self.parseFunctionParameters(&is_var_args);

        const parameter_subtypes = try self.allocator.alloc(SubType, parameter_subsymbols.len);

        for (parameter_subsymbols, 0..) |parameter_subsymbol, i| {
            parameter_subtypes[i] = parameter_subsymbol.subtype;
        }

        const return_subtype = if (self.peekToken().tag == .open_brace or (self.peekToken().tag == .semicolon and external))
            SubType{ .pure = .void }
        else
            try self.parseSubType();

        const return_subtype_on_heap = try self.allocator.create(SubType);
        return_subtype_on_heap.* = return_subtype;

        const function_subtype: SubType = .{
            .function = .{
                .parameter_subtypes = parameter_subtypes,
                .is_var_args = is_var_args,
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

        const subsymbol: SubSymbol = .{
            .name = name,
            .subtype = function_pointer_subtype,
        };

        if (external) {
            return self.sir.external_declarations.append(self.allocator, subsymbol);
        }

        const definition = try self.sir.functions.getOrPut(self.allocator, name.buffer);
        if (definition.found_existing) try self.reportRedeclaration(name);

        definition.value_ptr.* = .{
            .exported = exported,
            .subsymbol = subsymbol,
        };

        self.sir_instructions = &definition.value_ptr.instructions;

        try self.sir_instructions.append(self.allocator, .{ .block = 0 });

        self.block_id = 1;

        try self.sir_instructions.append(self.allocator, .start_scope);

        if (parameter_subsymbols.len != 0) {
            try self.sir_instructions.append(self.allocator, .{ .parameters = parameter_subsymbols });
        }

        try self.parseBody();

        if (self.sir_instructions.items.len == 0 or
            (self.sir_instructions.getLast() != .ret and
            self.sir_instructions.getLast() != .ret_void))
        {
            try self.sir_instructions.append(self.allocator, .{ .ret_void = fn_keyword_start });
        }

        try self.sir_instructions.append(self.allocator, .end_scope);
    }

    fn parseFunctionParameters(self: *Parser, is_var_args: *bool) Error![]SubSymbol {
        if (!self.eatToken(.open_paren)) {
            self.error_info = .{ .message = "expected a '('", .source_loc = SourceLoc.find(self.buffer, self.peekToken().range.start) };

            return error.UnexpectedToken;
        }

        var paramters: std.ArrayListUnmanaged(SubSymbol) = .{};

        while (!self.eatToken(.close_paren)) {
            if (self.peekToken().tag == .eof) {
                self.error_info = .{ .message = "expected a ')'", .source_loc = SourceLoc.find(self.buffer, self.peekToken().range.start) };

                return error.UnexpectedToken;
            }

            if (self.eatToken(.triple_period)) {
                is_var_args.* = true;

                if (self.peekToken().tag != .close_paren) {
                    self.error_info = .{ .message = "expected a ')'", .source_loc = SourceLoc.find(self.buffer, self.peekToken().range.start) };

                    return error.UnexpectedToken;
                }

                continue;
            }

            try paramters.append(self.allocator, .{
                .name = try self.parseName(),
                .subtype = try self.parseSubType(),
            });

            if (!self.eatToken(.comma) and self.peekToken().tag != .close_paren) {
                self.error_info = .{ .message = "expected a ',' after parameter", .source_loc = SourceLoc.find(self.buffer, self.peekToken().range.start) };

                return error.UnexpectedToken;
            }
        }

        return paramters.toOwnedSlice(self.allocator);
    }

    fn parseVariableDeclaration(
        self: *Parser,
        comptime global: bool,
        comptime external: bool,
        comptime exported: bool,
    ) Error!void {
        const init_token = self.nextToken();

        const is_const = init_token.tag == .keyword_const;

        const name = try self.parseName();

        const maybe_subtype = if (self.peekToken().tag == .assign or is_const)
            null
        else
            try self.parseSubType();

        const subsymbol: SubSymbol = .{
            .name = name,
            .subtype = maybe_subtype orelse .{ .pure = .void },
        };

        if (external) {
            return self.sir.external_declarations.append(self.allocator, subsymbol);
        }

        const has_initializer = self.peekToken().tag != .semicolon or is_const;

        if (has_initializer and !self.eatToken(.assign)) {
            self.error_info = .{ .message = "expected a '='", .source_loc = SourceLoc.find(self.buffer, self.peekToken().range.start) };

            return error.UnexpectedToken;
        }

        if (global) {
            const definition = if (is_const)
                try self.sir.global_constants.getOrPut(self.allocator, name.buffer)
            else
                try self.sir.global_variables.getOrPut(self.allocator, name.buffer);

            if (definition.found_existing) try self.reportRedeclaration(name);

            definition.value_ptr.* = .{
                .exported = exported,
                .subsymbol = subsymbol,
            };

            self.sir_instructions = &definition.value_ptr.instructions;
        }

        if (has_initializer) {
            try self.parseExpr(.lowest);
        }

        if (!global) {
            try self.sir_instructions.append(
                self.allocator,
                if (maybe_subtype != null)
                    .{
                        .variable = subsymbol,
                    }
                else if (is_const)
                    .{
                        .constant = subsymbol,
                    }
                else
                    .{
                        .variable_infer = subsymbol,
                    },
            );

            if (!is_const and has_initializer) {
                try self.sir_instructions.append(self.allocator, .{ .set = name });
            }
        }
    }

    fn parseTypeAlias(self: *Parser) Error!void {
        _ = self.nextToken();

        const name = try self.parseName();

        if (!self.eatToken(.assign)) {
            self.error_info = .{ .message = "expected a '='", .source_loc = SourceLoc.find(self.buffer, self.peekToken().range.start) };

            return error.UnexpectedToken;
        }

        const subtype = try self.parseSubType();

        const subsymbol: SubSymbol = .{
            .name = name,
            .subtype = subtype,
        };

        const type_alias = try self.sir.type_aliases.getOrPutValue(self.allocator, name.buffer, subsymbol);
        if (type_alias.found_existing) try self.reportRedeclaration(name);
    }

    fn popScopeDefers(self: *Parser) Error!void {
        while (self.scope_defer_count > 0) : (self.scope_defer_count -= 1) {
            const defer_instructions = self.defer_stack.pop();
            try self.sir_instructions.appendSlice(self.allocator, defer_instructions);
            self.allocator.free(defer_instructions);
        }
    }

    fn parseSwitch(self: *Parser) Error!void {
        const switch_keyword_start = self.nextToken().range.start;

        if (!self.eatToken(.open_paren)) {
            self.error_info = .{ .message = "expected a '('", .source_loc = SourceLoc.find(self.buffer, self.peekToken().range.start) };

            return error.UnexpectedToken;
        }

        try self.parseExpr(.lowest);

        if (!self.eatToken(.close_paren)) {
            self.error_info = .{ .message = "expected a ')'", .source_loc = SourceLoc.find(self.buffer, self.peekToken().range.start) };

            return error.UnexpectedToken;
        }

        if (!self.eatToken(.open_brace)) {
            self.error_info = .{ .message = "expected a '{'", .source_loc = SourceLoc.find(self.buffer, self.peekToken().range.start) };

            return error.UnexpectedToken;
        }

        var case_block_ids: std.ArrayListUnmanaged(u32) = .{};
        var case_token_starts: std.ArrayListUnmanaged(u32) = .{};
        var case_instructions: std.ArrayListUnmanaged(Instruction) = .{};

        var maybe_else_block_id: ?u32 = null;

        const end_block_id = self.block_id;
        self.block_id += 1;

        while (self.peekToken().tag != .eof and self.peekToken().tag != .close_brace) {
            const case_block_id = self.block_id;
            self.block_id += 1;

            if (self.peekToken().tag == .keyword_else) {
                const else_keyword_start = self.nextToken().range.start;

                if (maybe_else_block_id != null) {
                    self.error_info = .{ .message = "duplicate switch case", .source_loc = SourceLoc.find(self.buffer, else_keyword_start) };

                    return error.UnexpectedToken;
                }

                maybe_else_block_id = case_block_id;
            } else {
                try case_block_ids.append(self.allocator, case_block_id);
                try case_token_starts.append(self.allocator, self.peekToken().range.start);

                try self.parseExpr(.lowest);

                if (self.eatToken(.comma)) {
                    while (self.peekToken().tag != .eof and self.peekToken().tag != .fat_arrow) {
                        try case_block_ids.append(self.allocator, case_block_id);
                        try case_token_starts.append(self.allocator, self.peekToken().range.start);

                        try self.parseExpr(.lowest);

                        if (!self.eatToken(.comma) and self.peekToken().tag != .fat_arrow) {
                            self.error_info = .{ .message = "expected a ','", .source_loc = SourceLoc.find(self.buffer, self.peekToken().range.start) };

                            return error.UnexpectedToken;
                        }
                    }
                }
            }

            if (!self.eatToken(.fat_arrow)) {
                self.error_info = .{ .message = "expected a '=>'", .source_loc = SourceLoc.find(self.buffer, self.peekToken().range.start) };

                return error.UnexpectedToken;
            }

            try case_instructions.append(self.allocator, .{ .block = case_block_id });

            const previous_sir_instructions = self.sir_instructions;
            self.sir_instructions = &case_instructions;

            if (self.peekToken().tag == .open_brace) {
                try self.sir_instructions.append(self.allocator, .start_scope);

                try self.parseBody();

                try self.sir_instructions.append(self.allocator, .end_scope);
            } else {
                const previous_scope_defer_count = self.scope_defer_count;
                self.scope_defer_count = 0;

                try self.parseStmt(false);

                try self.popScopeDefers();
                self.scope_defer_count = previous_scope_defer_count;
            }

            try self.sir_instructions.append(self.allocator, .{ .br = end_block_id });

            self.sir_instructions = previous_sir_instructions;

            if (!self.eatToken(.comma) and self.peekToken().tag != .close_brace) {
                self.error_info = .{ .message = "expected a ','", .source_loc = SourceLoc.find(self.buffer, self.peekToken().range.start) };

                return error.UnexpectedToken;
            }
        }

        if (!self.eatToken(.close_brace)) {
            self.error_info = .{ .message = "expected a '}'", .source_loc = SourceLoc.find(self.buffer, self.peekToken().range.start) };

            return error.UnexpectedToken;
        }

        const owned_case_block_ids = try case_block_ids.toOwnedSlice(self.allocator);
        const owned_case_token_starts = try case_token_starts.toOwnedSlice(self.allocator);

        try self.sir_instructions.append(self.allocator, .{ .reverse = @intCast(owned_case_block_ids.len + 1) });

        if (maybe_else_block_id) |else_block_id| {
            try self.sir_instructions.append(self.allocator, .{
                .@"switch" = .{
                    .case_block_ids = owned_case_block_ids,
                    .case_token_starts = owned_case_token_starts,
                    .else_block_id = else_block_id,
                    .token_start = switch_keyword_start,
                },
            });
        } else {
            self.error_info = .{ .message = "unhandled switch cases (note: missing an 'else' case)", .source_loc = SourceLoc.find(self.buffer, switch_keyword_start) };

            return error.UnhandledSwitchCases;
        }

        try self.sir_instructions.appendSlice(self.allocator, case_instructions.items);

        case_instructions.deinit(self.allocator);

        try self.sir_instructions.append(self.allocator, .{ .block = end_block_id });
    }

    fn parseConditional(self: *Parser) Error!void {
        const end_block_id = self.block_id;
        self.block_id += 1;

        try self.sir_instructions.append(self.allocator, .{ .br = self.block_id });

        while (true) {
            const if_keyword_start = self.nextToken().range.start;

            try self.sir_instructions.append(self.allocator, .{ .block = self.block_id });
            self.block_id += 1;

            try self.parseExpr(.lowest);

            try self.sir_instructions.append(self.allocator, .{
                .cond_br = .{
                    .true_id = self.block_id,
                    .false_id = undefined,
                    .token_start = if_keyword_start,
                },
            });

            const cond_br_index = self.sir_instructions.items.len - 1;

            try self.sir_instructions.append(self.allocator, .{ .block = self.block_id });
            self.block_id += 1;

            try self.sir_instructions.append(self.allocator, .start_scope);

            try self.parseBody();

            self.sir_instructions.items[cond_br_index].cond_br.false_id = self.block_id;

            try self.sir_instructions.append(self.allocator, .{ .br = end_block_id });

            try self.sir_instructions.append(self.allocator, .end_scope);

            if (self.eatToken(.keyword_else)) {
                if (self.peekToken().tag == .keyword_if) continue;

                try self.sir_instructions.append(self.allocator, .{ .block = self.block_id });
                self.block_id += 1;

                try self.sir_instructions.append(self.allocator, .start_scope);

                try self.parseBody();

                try self.sir_instructions.append(self.allocator, .{ .br = end_block_id });

                try self.sir_instructions.append(self.allocator, .end_scope);
            } else {
                try self.sir_instructions.append(self.allocator, .{ .block = self.block_id });
                self.block_id += 1;

                try self.sir_instructions.append(self.allocator, .{ .br = end_block_id });
            }

            break;
        }

        try self.sir_instructions.append(self.allocator, .{ .block = end_block_id });
    }

    var maybe_header_block_id: ?u32 = null;
    var maybe_end_block_id: ?u32 = null;

    fn parseWhileLoop(self: *Parser) Error!void {
        const while_keyword_start = self.nextToken().range.start;

        const header_block_id = self.block_id;
        self.block_id += 1;

        const previous_header_block_id = maybe_header_block_id;
        maybe_header_block_id = header_block_id;
        defer maybe_header_block_id = previous_header_block_id;

        const body_block_id = self.block_id;
        self.block_id += 1;

        const end_block_id = self.block_id;
        self.block_id += 1;

        const previous_end_block_id = maybe_end_block_id;
        maybe_end_block_id = end_block_id;
        defer maybe_end_block_id = previous_end_block_id;

        try self.sir_instructions.append(self.allocator, .{ .br = header_block_id });

        try self.sir_instructions.append(self.allocator, .{ .block = header_block_id });

        try self.parseExpr(.lowest);

        try self.sir_instructions.append(self.allocator, .{ .cond_br = .{
            .true_id = body_block_id,
            .false_id = end_block_id,
            .token_start = while_keyword_start,
        } });

        try self.sir_instructions.append(self.allocator, .{ .block = body_block_id });

        try self.sir_instructions.append(self.allocator, .start_scope);

        try self.parseBody();

        try self.sir_instructions.append(self.allocator, .{ .br = header_block_id });

        try self.sir_instructions.append(self.allocator, .end_scope);

        try self.sir_instructions.append(self.allocator, .{ .block = end_block_id });
    }

    fn parseContinue(self: *Parser) Error!void {
        const continue_keyword_start = self.nextToken().range.start;

        if (maybe_header_block_id) |header_block_id| {
            return self.sir_instructions.append(self.allocator, .{ .br = header_block_id });
        }

        self.error_info = .{ .message = "expected the continue statement to be inside a loop", .source_loc = SourceLoc.find(self.buffer, continue_keyword_start) };

        return error.UnexpectedStatement;
    }

    fn parseBreak(self: *Parser) Error!void {
        const break_keyword_start = self.nextToken().range.start;

        if (maybe_end_block_id) |end_block_id| {
            return self.sir_instructions.append(self.allocator, .{ .br = end_block_id });
        }

        self.error_info = .{ .message = "expected the break statement to be inside a loop", .source_loc = SourceLoc.find(self.buffer, break_keyword_start) };

        return error.UnexpectedStatement;
    }

    fn parseDefer(self: *Parser) Error!void {
        _ = self.nextToken();

        const defer_instructions_start = self.sir_instructions.items.len;

        if (self.peekToken().tag == .open_brace) {
            try self.sir_instructions.append(self.allocator, .start_scope);

            try self.parseBody();

            try self.sir_instructions.append(self.allocator, .end_scope);
        } else {
            const previous_scope_defer_count = self.scope_defer_count;
            self.scope_defer_count = 0;

            try self.parseStmt(true);

            try self.popScopeDefers();
            self.scope_defer_count = previous_scope_defer_count;
        }

        if (defer_instructions_start == self.sir_instructions.items.len) return;

        const defer_instructions = self.sir_instructions.items[defer_instructions_start..];
        const defer_instructions_on_heap = try self.allocator.alloc(Instruction, defer_instructions.len);
        @memcpy(defer_instructions_on_heap, defer_instructions);

        try self.defer_stack.append(self.allocator, defer_instructions_on_heap);
        self.scope_defer_count += 1;

        self.sir_instructions.shrinkRetainingCapacity(defer_instructions_start);
    }

    fn parseReturn(self: *Parser) Error!void {
        const return_keyword_start = self.nextToken().range.start;

        const returns_value = self.peekToken().tag != .semicolon;

        if (returns_value) {
            try self.parseExpr(.lowest);
        }

        if (self.defer_stack.items.len > 0) {
            var i = self.defer_stack.items.len - 1;

            while (true) : (i -= 1) {
                const defer_instructions = self.defer_stack.items[i];
                try self.sir_instructions.appendSlice(self.allocator, defer_instructions);
                if (i == 0) break;
            }
        }

        if (returns_value) {
            try self.sir_instructions.append(self.allocator, .{ .ret = return_keyword_start });
        } else {
            try self.sir_instructions.append(self.allocator, .{ .ret_void = return_keyword_start });
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
        field,

        fn from(token: Token) Precedence {
            return switch (token.tag) {
                .plus_assign,
                .minus_assign,
                .star_assign,
                .divide_assign,
                .modulo_assign,
                .bit_and_assign,
                .bit_or_assign,
                .bit_xor_assign,
                .left_shift_assign,
                .right_shift_assign,
                .assign,
                => .assign,
                .less_than, .greater_than, .eql, .not_eql => .comparison,
                .plus, .minus => .sum,
                .star, .divide, .modulo => .product,
                .bit_and => .bit_and,
                .bit_or => .bit_or,
                .bit_xor => .bit_xor,
                .left_shift, .right_shift => .shift,
                .keyword_as => .cast,
                .open_paren => .call,
                .open_bracket => .subscript,
                .period => .field,

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

            .keyword_asm => try self.parseInlineAssembly(),

            .open_paren => try self.parseParentheses(),

            .minus, .bool_not, .bit_not, .bit_and => try self.parseUnaryOperation(),

            else => {
                self.error_info = .{ .message = "expected a statement or an expression", .source_loc = SourceLoc.find(self.buffer, self.peekToken().range.start) };

                return error.UnexpectedToken;
            },
        }
    }

    fn parseIdentifier(self: *Parser) Error!void {
        try self.sir_instructions.append(self.allocator, .{ .get = try self.parseName() });
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

                        '\'' => {
                            output.appendAssumeCapacity('\'');
                        },

                        else => return error.InvalidEscapeCharacter,
                    }
                },
            }
        }
    }

    fn parseString(self: *Parser) Error!void {
        const content = self.tokenValue(self.peekToken());
        const string_start = self.nextToken().range.start;

        var unescaped_list = std.ArrayList(u8).init(self.allocator);

        unescape(&unescaped_list, content) catch |err| switch (err) {
            error.InvalidEscapeCharacter => {
                self.error_info = .{ .message = "invalid escape character", .source_loc = SourceLoc.find(self.buffer, string_start) };

                return error.InvalidString;
            },

            inline else => |other_err| return other_err,
        };

        const unescaped = try unescaped_list.toOwnedSlice();

        try self.sir_instructions.append(self.allocator, .{ .string = unescaped });
    }

    fn parseChar(self: *Parser) Error!void {
        const encoded = self.tokenValue(self.peekToken());
        const char_start = self.nextToken().range.start;

        var unescaped_list = std.ArrayList(u8).init(self.allocator);

        unescape(&unescaped_list, encoded) catch |err| switch (err) {
            error.InvalidEscapeCharacter => {
                self.error_info = .{ .message = "invalid escape character", .source_loc = SourceLoc.find(self.buffer, char_start) };

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
            self.error_info = .{ .message = "invalid character literal", .source_loc = SourceLoc.find(self.buffer, char_start) };

            return error.InvalidChar;
        };

        try self.sir_instructions.append(self.allocator, .{ .int = decoded });
    }

    fn parseInt(self: *Parser) Error!void {
        const value = std.fmt.parseInt(i128, self.tokenValue(self.nextToken()), 0) catch {
            self.error_info = .{ .message = "invalid number", .source_loc = SourceLoc.find(self.buffer, self.peekToken().range.start) };

            return error.InvalidNumber;
        };

        try self.sir_instructions.append(self.allocator, .{ .int = value });
    }

    fn parseFloat(self: *Parser) Error!void {
        const value = std.fmt.parseFloat(f64, self.tokenValue(self.nextToken())) catch {
            self.error_info = .{ .message = "invalid number", .source_loc = SourceLoc.find(self.buffer, self.peekToken().range.start) };

            return error.InvalidNumber;
        };

        try self.sir_instructions.append(self.allocator, .{ .float = value });
    }

    fn parseGlobalAssembly(self: *Parser) Error!void {
        _ = self.nextToken();

        if (!self.eatToken(.open_brace)) {
            self.error_info = .{ .message = "expected a '{'", .source_loc = SourceLoc.find(self.buffer, self.peekToken().range.start) };

            return error.UnexpectedToken;
        }

        while (!self.eatToken(.close_brace)) {
            if (self.peekToken().tag != .string_literal) {
                self.error_info = .{ .message = "expected a valid string", .source_loc = SourceLoc.find(self.buffer, self.peekToken().range.start) };

                return error.UnexpectedToken;
            }

            const content = self.tokenValue(self.peekToken());
            const string_start = self.nextToken().range.start;

            var global_assembly_managed = self.sir.global_assembly.toManaged(self.allocator);

            unescape(&global_assembly_managed, content) catch |err| switch (err) {
                error.InvalidEscapeCharacter => {
                    self.error_info = .{ .message = "invalid escape character", .source_loc = SourceLoc.find(self.buffer, string_start) };

                    return error.InvalidString;
                },

                inline else => |other_err| return other_err,
            };

            self.sir.global_assembly = global_assembly_managed.moveToUnmanaged();

            try self.sir.global_assembly.append(self.allocator, '\n');
        }
    }

    fn parseInlineAssembly(self: *Parser) Error!void {
        const asm_keyword_start = self.nextToken().range.start;

        var content: std.ArrayListUnmanaged(u8) = .{};

        if (!self.eatToken(.open_brace)) {
            self.error_info = .{ .message = "expected a '{'", .source_loc = SourceLoc.find(self.buffer, self.peekToken().range.start) };

            return error.UnexpectedToken;
        }

        var input_constraints: []const []const u8 = &.{};
        var output_constraint: ?Instruction.InlineAssembly.OutputConstraint = null;
        var clobbers: []const []const u8 = &.{};

        while (!self.eatToken(.close_brace)) {
            if (self.peekToken().tag != .string_literal) {
                self.error_info = .{ .message = "expected a valid string", .source_loc = SourceLoc.find(self.buffer, self.peekToken().range.start) };

                return error.UnexpectedToken;
            }

            try self.parseString();

            try content.appendSlice(self.allocator, self.sir_instructions.pop().string);
            try content.append(self.allocator, '\n');

            if (self.eatToken(.colon)) {
                if (self.peekToken().tag != .colon) {
                    output_constraint = try self.parseInlineAssemblyOutputConstraint();
                }
            }

            if (self.eatToken(.colon)) {
                if (self.peekToken().tag != .colon) {
                    input_constraints = try self.parseInlineAssemblyInputConstraints();
                }
            }

            if (self.eatToken(.colon)) {
                if (self.peekToken().tag != .close_brace) {
                    clobbers = try self.parseInlineAssemblyClobbers();
                }
            }

            if (self.peekToken().tag != .close_brace and (self.peekToken().tag != .colon or self.peekToken().tag != .string_literal)) {
                self.error_info = .{ .message = "expected a '}'", .source_loc = SourceLoc.find(self.buffer, self.peekToken().range.start) };

                return error.UnexpectedToken;
            }
        }

        try self.sir_instructions.append(self.allocator, .{
            .inline_assembly = .{
                .content = try content.toOwnedSlice(self.allocator),
                .input_constraints = input_constraints,
                .output_constraint = output_constraint,
                .clobbers = clobbers,
                .token_start = asm_keyword_start,
            },
        });
    }

    fn parseInlineAssemblyInputConstraints(self: *Parser) Error![][]const u8 {
        var constraints = std.ArrayList([]const u8).init(self.allocator);

        while (self.peekToken().tag != .eof and self.peekToken().tag != .colon and self.peekToken().tag != .close_brace) {
            try constraints.append(try self.parseInlineAssemblyInputConstraint());

            if (!self.eatToken(.comma) and self.peekToken().tag != .colon and self.peekToken().tag != .close_brace) {
                self.error_info = .{ .message = "expected a ','", .source_loc = SourceLoc.find(self.buffer, self.peekToken().range.start) };

                return error.UnexpectedToken;
            }
        }

        try self.sir_instructions.append(self.allocator, .{ .reverse = @intCast(constraints.items.len) });

        return constraints.toOwnedSlice();
    }

    fn parseInlineAssemblyInputConstraint(self: *Parser) Error![]const u8 {
        if (self.peekToken().tag != .string_literal) {
            self.error_info = .{ .message = "expected a valid string", .source_loc = SourceLoc.find(self.buffer, self.peekToken().range.start) };

            return error.UnexpectedToken;
        }

        try self.parseString();

        const register = self.sir_instructions.pop().string;

        if (!self.eatToken(.open_paren)) {
            self.error_info = .{ .message = "expected a '('", .source_loc = SourceLoc.find(self.buffer, self.peekToken().range.start) };

            return error.UnexpectedToken;
        }

        try self.parseExpr(.lowest);

        if (!self.eatToken(.close_paren)) {
            self.error_info = .{ .message = "expected a ')'", .source_loc = SourceLoc.find(self.buffer, self.peekToken().range.start) };

            return error.UnexpectedToken;
        }

        return register;
    }

    fn parseInlineAssemblyOutputConstraint(self: *Parser) Error!Instruction.InlineAssembly.OutputConstraint {
        if (self.peekToken().tag != .string_literal) {
            self.error_info = .{ .message = "expected a valid string", .source_loc = SourceLoc.find(self.buffer, self.peekToken().range.start) };

            return error.UnexpectedToken;
        }

        try self.parseString();

        const register = self.sir_instructions.pop().string;

        if (!self.eatToken(.open_paren)) {
            self.error_info = .{ .message = "expected a '('", .source_loc = SourceLoc.find(self.buffer, self.peekToken().range.start) };

            return error.UnexpectedToken;
        }

        const subtype = try self.parseSubType();

        if (!self.eatToken(.close_paren)) {
            self.error_info = .{ .message = "expected a ')'", .source_loc = SourceLoc.find(self.buffer, self.peekToken().range.start) };

            return error.UnexpectedToken;
        }

        return Instruction.InlineAssembly.OutputConstraint{
            .register = register,
            .subtype = subtype,
        };
    }

    fn parseInlineAssemblyClobbers(self: *Parser) Error![][]const u8 {
        var clobbers = std.ArrayList([]const u8).init(self.allocator);

        while (self.peekToken().tag != .eof and self.peekToken().tag != .close_brace) {
            if (self.peekToken().tag != .string_literal) {
                self.error_info = .{ .message = "expected a valid string", .source_loc = SourceLoc.find(self.buffer, self.peekToken().range.start) };

                return error.UnexpectedToken;
            }

            try self.parseString();

            try clobbers.append(self.sir_instructions.pop().string);

            if (!self.eatToken(.comma) and self.peekToken().tag != .close_brace) {
                self.error_info = .{ .message = "expected a ','", .source_loc = SourceLoc.find(self.buffer, self.peekToken().range.start) };

                return error.UnexpectedToken;
            }
        }

        return clobbers.toOwnedSlice();
    }

    fn parseParentheses(self: *Parser) Error!void {
        _ = self.nextToken();

        try self.parseExpr(.lowest);

        if (!self.eatToken(.close_paren)) {
            self.error_info = .{ .message = "expected a ')'", .source_loc = SourceLoc.find(self.buffer, self.peekToken().range.start) };

            return error.UnexpectedToken;
        }
    }

    fn parseUnaryOperation(self: *Parser) Error!void {
        const operator_token = self.nextToken();

        try self.parseExpr(.prefix);

        switch (operator_token.tag) {
            .minus => {
                try self.sir_instructions.append(self.allocator, .{ .negate = operator_token.range.start });
            },

            .bool_not => {
                try self.sir_instructions.append(self.allocator, .{ .bool_not = operator_token.range.start });
            },

            .bit_not => {
                try self.sir_instructions.append(self.allocator, .{ .bit_not = operator_token.range.start });
            },

            .bit_and => {
                try self.sir_instructions.append(self.allocator, .reference);
            },

            else => unreachable,
        }
    }

    fn parseBinaryExpr(self: *Parser) Error!void {
        switch (self.peekToken().tag) {
            .plus,
            .minus,
            .star,
            .divide,
            .modulo,
            .less_than,
            .greater_than,
            .left_shift,
            .right_shift,
            .bit_and,
            .bit_or,
            .bit_xor,
            .plus_assign,
            .minus_assign,
            .star_assign,
            .divide_assign,
            .modulo_assign,
            .bit_and_assign,
            .bit_or_assign,
            .bit_xor_assign,
            .left_shift_assign,
            .right_shift_assign,
            .assign,
            .eql,
            .not_eql,
            => try self.parseBinaryOperation(),

            .keyword_as => try self.parseCast(),

            .open_paren => try self.parseCall(),

            .open_bracket => try self.parseSubscript(),

            .period => try self.parseFieldAccess(),

            else => {
                self.error_info = .{ .message = "expected a statement or an expression", .source_loc = SourceLoc.find(self.buffer, self.peekToken().range.start) };

                return error.UnexpectedToken;
            },
        }
    }

    fn parseBinaryOperation(self: *Parser) Error!void {
        const operator_token = self.nextToken();

        const precedence = Precedence.from(operator_token);

        if (precedence != .assign) {
            try self.parseExpr(precedence);
        }

        switch (operator_token.tag) {
            .plus_assign,
            .minus_assign,
            .star_assign,
            .divide_assign,
            .modulo_assign,
            .bit_and_assign,
            .bit_or_assign,
            .bit_xor_assign,
            .left_shift_assign,
            .right_shift_assign,
            .assign,
            => {
                const maybe_additional_operator_token: ?Token = if (operator_token.tag != .assign)
                    .{
                        .tag = switch (operator_token.tag) {
                            .plus_assign => .plus,
                            .minus_assign => .minus,
                            .star_assign => .star,
                            .divide_assign => .divide,
                            .modulo_assign => .modulo,
                            .bit_and_assign => .bit_and,
                            .bit_or_assign => .bit_or,
                            .bit_xor_assign => .bit_xor,
                            .left_shift_assign => .left_shift,
                            .right_shift_assign => .right_shift,

                            else => unreachable,
                        },
                        .range = operator_token.range,
                    }
                else
                    null;

                const last_instruction = self.sir_instructions.pop();

                if (last_instruction == .get_element or last_instruction == .get_field) {
                    try self.sir_instructions.appendSlice(self.allocator, &.{ last_instruction, .reference });

                    try self.parseExpr(precedence);

                    if (maybe_additional_operator_token) |additional_operator_token| {
                        try self.sir_instructions.appendSlice(
                            self.allocator,
                            &.{
                                .{ .reverse = 2 },
                                .duplicate,
                                .{ .read = operator_token.range.start },
                                .{ .reverse = 2 },
                                .{ .reverse = 3 },
                            },
                        );

                        try self.emitBinaryOperation(additional_operator_token);
                    }

                    try self.sir_instructions.appendSlice(
                        self.allocator,
                        &.{
                            .duplicate,
                            .{ .reverse = 3 },
                            .{ .write = operator_token.range.start },
                        },
                    );
                } else if (last_instruction == .read) {
                    try self.parseExpr(precedence);

                    if (maybe_additional_operator_token) |additional_operator_token| {
                        try self.sir_instructions.appendSlice(
                            self.allocator,
                            &.{
                                .{ .reverse = 2 },
                                .duplicate,
                                .{ .read = operator_token.range.start },
                                .{ .reverse = 2 },
                                .{ .reverse = 3 },
                            },
                        );

                        try self.emitBinaryOperation(additional_operator_token);
                    }

                    try self.sir_instructions.appendSlice(
                        self.allocator,
                        &.{
                            .duplicate,
                            .{ .reverse = 3 },
                            .{ .write = operator_token.range.start },
                        },
                    );
                } else if (last_instruction == .get) {
                    try self.parseExpr(precedence);

                    if (maybe_additional_operator_token) |additional_operator_token| {
                        try self.sir_instructions.appendSlice(
                            self.allocator,
                            &.{
                                last_instruction,
                                .{ .reverse = 2 },
                            },
                        );

                        try self.emitBinaryOperation(additional_operator_token);
                    }

                    try self.sir_instructions.appendSlice(
                        self.allocator,
                        &.{
                            .duplicate,
                            .{ .set = last_instruction.get },
                        },
                    );
                } else {
                    self.error_info = .{ .message = "cannot assign to value", .source_loc = SourceLoc.find(self.buffer, operator_token.range.start) };

                    return error.UnexpectedExpression;
                }
            },

            else => try self.emitBinaryOperation(operator_token),
        }
    }

    fn emitBinaryOperation(self: *Parser, operator_token: Token) Error!void {
        switch (operator_token.tag) {
            .eql, .not_eql => {
                try self.sir_instructions.append(self.allocator, .{ .eql = operator_token.range.start });

                if (operator_token.tag == .not_eql) {
                    try self.sir_instructions.append(self.allocator, .{ .bool_not = operator_token.range.start });
                }
            },

            .plus => try self.sir_instructions.append(self.allocator, .{ .add = operator_token.range.start }),
            .minus => try self.sir_instructions.append(self.allocator, .{ .sub = operator_token.range.start }),
            .star => try self.sir_instructions.append(self.allocator, .{ .mul = operator_token.range.start }),
            .divide => try self.sir_instructions.append(self.allocator, .{ .div = operator_token.range.start }),
            .modulo => try self.sir_instructions.append(self.allocator, .{ .rem = operator_token.range.start }),
            .less_than => try self.sir_instructions.append(self.allocator, .{ .lt = operator_token.range.start }),
            .greater_than => try self.sir_instructions.append(self.allocator, .{ .gt = operator_token.range.start }),
            .left_shift => try self.sir_instructions.append(self.allocator, .{ .shl = operator_token.range.start }),
            .right_shift => try self.sir_instructions.append(self.allocator, .{ .shr = operator_token.range.start }),
            .bit_and => try self.sir_instructions.append(self.allocator, .{ .bit_and = operator_token.range.start }),
            .bit_or => try self.sir_instructions.append(self.allocator, .{ .bit_or = operator_token.range.start }),
            .bit_xor => try self.sir_instructions.append(self.allocator, .{ .bit_xor = operator_token.range.start }),

            else => unreachable,
        }
    }

    fn parseCast(self: *Parser) Error!void {
        const as_keyword_start = self.nextToken().range.start;

        const to = try self.parseSubType();

        try self.sir_instructions.append(self.allocator, .{ .cast = .{ .to = to, .token_start = as_keyword_start } });
    }

    fn parseCall(self: *Parser) Error!void {
        const open_paren_start = self.nextToken().range.start;

        const arguments_count = try self.parseCallArguments();

        try self.sir_instructions.append(self.allocator, .{ .reverse = arguments_count + 1 });

        try self.sir_instructions.append(self.allocator, .{ .call = .{ .arguments_count = arguments_count, .token_start = open_paren_start } });
    }

    fn parseCallArguments(self: *Parser) Error!u32 {
        var count: u32 = 0;

        while (self.peekToken().tag != .eof and self.peekToken().tag != .close_paren) {
            try self.parseExpr(.lowest);
            count += 1;

            if (!self.eatToken(.comma) and self.peekToken().tag != .close_paren) {
                self.error_info = .{ .message = "expected a ','", .source_loc = SourceLoc.find(self.buffer, self.peekToken().range.start) };

                return error.UnexpectedToken;
            }
        }

        if (!self.eatToken(.close_paren)) {
            self.error_info = .{ .message = "expected a ')'", .source_loc = SourceLoc.find(self.buffer, self.peekToken().range.start) };

            return error.UnexpectedToken;
        }

        return count;
    }

    fn parseSubscript(self: *Parser) Error!void {
        const open_bracket_start = self.nextToken().range.start;

        try self.sir_instructions.append(self.allocator, .{ .pre_get_element = open_bracket_start });

        try self.parseExpr(.lowest);

        const slicing = self.eatToken(.double_period);

        if (slicing) {
            try self.parseExpr(.lowest);

            try self.sir_instructions.append(self.allocator, .{ .make_slice = open_bracket_start });
        }

        if (!self.eatToken(.close_bracket)) {
            self.error_info = .{ .message = "expected a ']'", .source_loc = SourceLoc.find(self.buffer, self.peekToken().range.start) };

            return error.UnexpectedToken;
        }

        if (!slicing) {
            try self.sir_instructions.append(self.allocator, .{ .get_element = open_bracket_start });
        }
    }

    fn parseFieldAccess(self: *Parser) Error!void {
        const period_start = self.nextToken().range.start;

        if (self.eatToken(.star)) {
            try self.sir_instructions.append(self.allocator, .{ .read = period_start });
        } else {
            const name = try self.parseName();

            try self.sir_instructions.append(self.allocator, .{ .get_field = name });
        }
    }

    fn parseName(self: *Parser) Error!Name {
        if (self.peekToken().tag != .identifier) {
            self.error_info = .{ .message = "expected a name", .source_loc = SourceLoc.find(self.buffer, self.peekToken().range.start) };

            return error.UnexpectedToken;
        }

        return Name{ .buffer = self.tokenValue(self.peekToken()), .token_start = self.nextToken().range.start };
    }

    fn parseSubType(self: *Parser) Error!SubType {
        switch (self.peekToken().tag) {
            .identifier => {
                return SubType{ .name = try self.parseName() };
            },

            .keyword_struct => {
                _ = self.nextToken();

                if (!self.eatToken(.open_brace)) {
                    self.error_info = .{ .message = "expected a '{'", .source_loc = SourceLoc.find(self.buffer, self.peekToken().range.start) };

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

                        self.error_info = .{ .message = error_message_buf.items, .source_loc = SourceLoc.find(self.buffer, name.token_start) };

                        return error.Redeclared;
                    }

                    try fields_hashset.put(self.allocator, name.buffer, {});

                    try subsymbols.append(self.allocator, .{
                        .name = name,
                        .subtype = try self.parseSubType(),
                    });

                    if (!self.eatToken(.comma) and self.peekToken().tag != .close_brace) {
                        self.error_info = .{ .message = "expected a ','", .source_loc = SourceLoc.find(self.buffer, self.peekToken().range.start) };

                        return error.UnexpectedToken;
                    }
                }

                if (!self.eatToken(.close_brace)) {
                    self.error_info = .{ .message = "expected a '}'", .source_loc = SourceLoc.find(self.buffer, self.peekToken().range.start) };

                    return error.UnexpectedToken;
                }

                return SubType{ .@"struct" = .{ .subsymbols = try subsymbols.toOwnedSlice(self.allocator) } };
            },

            .keyword_enum => {
                const enum_keyword_start = self.nextToken().range.start;

                const maybe_subtype = if (self.peekToken().tag == .open_brace)
                    null
                else
                    try self.parseSubType();

                if (!self.eatToken(.open_brace)) {
                    self.error_info = .{ .message = "expected a '{'", .source_loc = SourceLoc.find(self.buffer, enum_keyword_start) };

                    return error.UnexpectedToken;
                }

                var fields: std.ArrayListUnmanaged(SubType.Enum.Field) = .{};

                var fields_hashset = std.StringHashMapUnmanaged(void){};
                defer fields_hashset.deinit(self.allocator);

                var next_value: i128 = 0;

                var max_value: i128 = 0;
                var min_value: i128 = 0;

                while (self.peekToken().tag != .eof and self.peekToken().tag != .close_brace) {
                    const name = try self.parseName();

                    if (fields_hashset.get(name.buffer)) |_| {
                        var error_message_buf: std.ArrayListUnmanaged(u8) = .{};

                        try error_message_buf.writer(self.allocator).print("redeclaration of '{s}' in enum fields", .{name.buffer});

                        self.error_info = .{ .message = error_message_buf.items, .source_loc = SourceLoc.find(self.buffer, name.token_start) };

                        return error.Redeclared;
                    }

                    try fields_hashset.put(self.allocator, name.buffer, {});

                    const value = if (self.eatToken(.assign)) blk: {
                        if (self.peekToken().tag != .int) {
                            self.error_info = .{ .message = "expected an integer", .source_loc = SourceLoc.find(self.buffer, self.peekToken().range.start) };

                            return error.UnexpectedToken;
                        }

                        break :blk std.fmt.parseInt(i128, self.tokenValue(self.nextToken()), 0) catch {
                            self.error_info = .{ .message = "invalid number", .source_loc = SourceLoc.find(self.buffer, self.peekToken().range.start) };

                            return error.InvalidNumber;
                        };
                    } else next_value;

                    next_value = value + 1;

                    if (value > max_value) max_value = value;
                    if (value < min_value) min_value = value;

                    try fields.append(self.allocator, .{
                        .name = name,
                        .value = value,
                    });

                    if (!self.eatToken(.comma) and self.peekToken().tag != .close_brace) {
                        self.error_info = .{ .message = "expected a ','", .source_loc = SourceLoc.find(self.buffer, self.peekToken().range.start) };

                        return error.UnexpectedToken;
                    }
                }

                if (!self.eatToken(.close_brace)) {
                    self.error_info = .{ .message = "expected a '}'", .source_loc = SourceLoc.find(self.buffer, self.peekToken().range.start) };

                    return error.UnexpectedToken;
                }

                const subtype = if (maybe_subtype) |subtype|
                    subtype
                else blk: {
                    break :blk SubType{ .pure = Type.intFittingRange(min_value, max_value) };
                };

                const subtype_on_heap = try self.allocator.create(SubType);
                subtype_on_heap.* = subtype;

                return SubType{
                    .@"enum" = .{
                        .subtype = subtype_on_heap,
                        .fields = try fields.toOwnedSlice(self.allocator),
                        .token_start = enum_keyword_start,
                    },
                };
            },

            .keyword_fn => {
                _ = self.nextToken();

                if (!self.eatToken(.open_paren)) {
                    self.error_info = .{ .message = "expected a '('", .source_loc = SourceLoc.find(self.buffer, self.peekToken().range.start) };

                    return error.UnexpectedToken;
                }

                var is_var_args = false;

                var parameter_subtypes: std.ArrayListUnmanaged(SubType) = .{};

                while (!self.eatToken(.close_paren)) {
                    if (self.peekToken().tag == .eof) {
                        self.error_info = .{ .message = "expected a ')'", .source_loc = SourceLoc.find(self.buffer, self.peekToken().range.start) };

                        return error.UnexpectedToken;
                    }

                    if (self.eatToken(.triple_period)) {
                        is_var_args = true;

                        if (self.peekToken().tag != .close_paren) {
                            self.error_info = .{ .message = "expected a ')'", .source_loc = SourceLoc.find(self.buffer, self.peekToken().range.start) };

                            return error.UnexpectedToken;
                        }

                        continue;
                    }

                    try parameter_subtypes.append(self.allocator, try self.parseSubType());

                    if (!self.eatToken(.comma) and self.peekToken().tag != .close_paren) {
                        self.error_info = .{ .message = "expected a ','", .source_loc = SourceLoc.find(self.buffer, self.peekToken().range.start) };

                        return error.UnexpectedToken;
                    }
                }

                const return_subtype = try self.parseSubType();

                const return_subtype_on_heap = try self.allocator.create(SubType);
                return_subtype_on_heap.* = return_subtype;

                return SubType{
                    .function = .{
                        .parameter_subtypes = try parameter_subtypes.toOwnedSlice(self.allocator),
                        .is_var_args = is_var_args,
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

                var size: Type.Pointer.Size = .slice;

                if (self.eatToken(.star)) {
                    if (!self.eatToken(.close_bracket)) {
                        self.error_info = .{ .message = "expected a ']'", .source_loc = SourceLoc.find(self.buffer, self.peekToken().range.start) };

                        return error.UnexpectedToken;
                    }

                    size = .many;
                } else if (self.peekToken().tag == .int) {
                    const len = std.fmt.parseInt(usize, self.tokenValue(self.nextToken()), 0) catch {
                        self.error_info = .{ .message = "invalid array length", .source_loc = SourceLoc.find(self.buffer, self.peekToken().range.start) };

                        return error.InvalidNumber;
                    };

                    if (!self.eatToken(.close_bracket)) {
                        self.error_info = .{ .message = "expected a ']'", .source_loc = SourceLoc.find(self.buffer, self.peekToken().range.start) };

                        return error.UnexpectedToken;
                    }

                    const child = try self.parseSubType();

                    const child_on_heap = try self.allocator.create(SubType);
                    child_on_heap.* = child;

                    return SubType{
                        .array = .{
                            .len = len,
                            .child_subtype = child_on_heap,
                        },
                    };
                } else if (!self.eatToken(.close_bracket)) {
                    self.error_info = .{ .message = "expected a ']'", .source_loc = SourceLoc.find(self.buffer, self.peekToken().range.start) };

                    return error.UnexpectedToken;
                }

                const is_const = self.eatToken(.keyword_const);

                const child = try self.parseSubType();

                const child_on_heap = try self.allocator.create(SubType);
                child_on_heap.* = child;

                return SubType{
                    .pointer = .{
                        .size = size,
                        .is_const = is_const,
                        .child_subtype = child_on_heap,
                    },
                };
            },

            else => {},
        }

        self.error_info = .{ .message = "expected a type", .source_loc = SourceLoc.find(self.buffer, self.peekToken().range.start) };

        return error.InvalidType;
    }

    fn parseBody(self: *Parser) Error!void {
        const previous_scope_defer_count = self.scope_defer_count;
        self.scope_defer_count = 0;

        if (!self.eatToken(.open_brace)) {
            self.error_info = .{ .message = "expected a '{'", .source_loc = SourceLoc.find(self.buffer, self.peekToken().range.start) };

            return error.UnexpectedToken;
        }

        while (!self.eatToken(.close_brace)) {
            try self.parseStmt(true);

            if (self.peekToken().tag == .eof) {
                self.error_info = .{ .message = "expected a '}'", .source_loc = SourceLoc.find(self.buffer, self.peekToken().range.start) };

                return error.UnexpectedToken;
            }
        }

        try self.popScopeDefers();

        self.scope_defer_count = previous_scope_defer_count;
    }

    fn reportRedeclaration(self: *Parser, name: Name) Error!noreturn {
        var error_message_buf: std.ArrayListUnmanaged(u8) = .{};

        try error_message_buf.writer(self.allocator).print("redeclaration of '{s}'", .{name.buffer});

        self.error_info = .{ .message = error_message_buf.items, .source_loc = SourceLoc.find(self.buffer, name.token_start) };

        return error.Redeclared;
    }
};
