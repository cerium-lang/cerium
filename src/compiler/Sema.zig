//! Semantic Analyzer.
//!
//! An analyzer that lowers down `Hir` to `Lir` while checking if the semantics are completely right.

const std = @import("std");

const Ast = @import("Ast.zig");
const Hir = @import("Hir.zig");
const Compilation = @import("Compilation.zig");
const Lir = @import("Lir.zig");
const Symbol = @import("Symbol.zig");
const Type = @import("Type.zig");

const Sema = @This();

allocator: std.mem.Allocator,

env: Compilation.Environment,

lir: Lir = .{},

maybe_lir_function: ?*Lir.Function = null,
maybe_hir_function: ?*Hir.Function = null,

lir_block: *Lir.Block = undefined,

stack: std.ArrayListUnmanaged(Value) = .{},

scope_stack: std.ArrayListUnmanaged(Symbol.Scope(Variable)) = .{},
scope: *Symbol.Scope(Variable) = undefined,

error_info: ?ErrorInfo = null,

pub const ErrorInfo = struct {
    message: []const u8,
    source_loc: Ast.SourceLoc,
};

pub const Error = error{
    ExpectedCompiletimeConstant,
    UnexpectedArgumentsCount,
    ExpectedExplicitReturn,
    UnexpectedMutation,
    UnexpectedAssemblyConstraints,
    TypeCannotRepresentValue,
    UnexpectedType,
    MismatchedTypes,
    Undeclared,
} || std.mem.Allocator.Error;

const Value = union(enum) {
    string: []const u8,
    int: i128,
    float: f64,
    boolean: bool,
    runtime: Runtime,

    const Runtime = struct {
        type: Type,
        data: Data = .none,

        const Data = union(enum) {
            none,
            name: Ast.Name,
        };
    };

    fn canImplicitCast(self: Value, to: Type) bool {
        const self_type = self.getType();

        return (self == .int and to.isInt()) or
            (self == .float and to.isFloat()) or
            (to.tag == .ambigiuous_int and self_type.isInt()) or
            (to.tag == .ambigiuous_float and self_type.isFloat()) or
            (self_type.isInt() and to.isInt() and
            self_type.maxInt() <= to.maxInt() and self_type.minInt() >= to.minInt() and
            self_type.canBeNegative() == to.canBeNegative()) or
            (self_type.isFloat() and to.isFloat() and
            self_type.maxFloat() <= to.maxFloat() and self_type.minFloat() >= to.minFloat()) or
            self_type.eql(to);
    }

    fn canBeRepresented(self: Value, as: Type) bool {
        return (self == .int and self.int >= as.minInt() and
            self == .int and self.int <= as.maxInt()) or
            (self == .float and self.float >= as.minFloat() and
            self == .float and self.float <= as.maxFloat()) or
            (self == .boolean) or
            (self == .string) or
            (self == .runtime);
    }

    fn getType(self: Value) Type {
        return switch (self) {
            .int => Type{ .tag = .ambigiuous_int },
            .float => Type{ .tag = .ambigiuous_float },
            .boolean => Type{ .tag = .bool },
            .string => Type{
                .tag = .pointer,
                .data = .{
                    .pointer = .{
                        .size = .many,
                        .is_const = true,
                        .is_local = false,
                        .child_type = &.{ .tag = .u8 },
                    },
                },
            },
            .runtime => |runtime| runtime.type,
        };
    }

    pub fn format(self: Value, _: anytype, _: anytype, writer: anytype) !void {
        switch (self) {
            .int => |int| try writer.print("{}", .{int}),
            .float => |float| try writer.print("{d}", .{float}),
            .boolean => |boolean| try writer.print("{}", .{boolean}),
            .string => |string| try writer.print("{s}", .{string}),
            .runtime => |runtime| try writer.print("<runtime value '{}'>", .{runtime.type}),
        }
    }
};

const Variable = struct {
    symbol: Symbol,
    is_const: bool = false,
    is_comptime: bool = false,
    maybe_value: ?Value = null,
};

pub fn init(allocator: std.mem.Allocator, env: Compilation.Environment) Sema {
    return Sema{
        .allocator = allocator,
        .env = env,
    };
}

fn checkRepresentability(self: *Sema, source_value: Value, destination_type: Type, source_loc: Ast.SourceLoc) Error!void {
    const source_type = source_value.getType();

    if (!source_value.canImplicitCast(destination_type)) {
        var error_message_buf: std.ArrayListUnmanaged(u8) = .{};

        try error_message_buf.writer(self.allocator).print("'{}' cannot be implicitly casted to '{}'", .{ source_type, destination_type });

        self.error_info = .{ .message = error_message_buf.items, .source_loc = source_loc };

        return error.MismatchedTypes;
    }

    if (!source_value.canBeRepresented(destination_type)) {
        var error_message_buf: std.ArrayListUnmanaged(u8) = .{};

        try error_message_buf.writer(self.allocator).print("'{}' cannot represent value '{}'", .{ destination_type, source_value });

        self.error_info = .{ .message = error_message_buf.items, .source_loc = source_loc };

        return error.TypeCannotRepresentValue;
    }

    if (source_type.isLocalPointer() and !destination_type.isLocalPointer()) {
        var error_message_buf: std.ArrayListUnmanaged(u8) = .{};

        try error_message_buf.writer(self.allocator).print("'{}' is pointing to data that is local to this function and therefore cannot escape globally", .{source_type});

        self.error_info = .{ .message = error_message_buf.items, .source_loc = source_loc };

        return error.MismatchedTypes;
    }
}

fn checkIntOrFloat(self: *Sema, provided_type: Type, source_loc: Ast.SourceLoc) Error!void {
    if (!provided_type.isIntOrFloat()) {
        var error_message_buf: std.ArrayListUnmanaged(u8) = .{};

        try error_message_buf.writer(self.allocator).print("'{}' is provided while expected an integer or float", .{provided_type});
        self.error_info = .{ .message = error_message_buf.items, .source_loc = source_loc };

        return error.MismatchedTypes;
    }
}

fn checkIntOrFloatOrPointer(self: *Sema, provided_type: Type, source_loc: Ast.SourceLoc) Error!void {
    if (!provided_type.isIntOrFloatOrPointer()) {
        var error_message_buf: std.ArrayListUnmanaged(u8) = .{};

        try error_message_buf.writer(self.allocator).print("'{}' is provided while expected an integer or float or pointer", .{provided_type});

        self.error_info = .{ .message = error_message_buf.items, .source_loc = source_loc };

        return error.MismatchedTypes;
    }
}

fn checkInt(self: *Sema, provided_type: Type, source_loc: Ast.SourceLoc) Error!void {
    if (!provided_type.isInt()) {
        var error_message_buf: std.ArrayListUnmanaged(u8) = .{};

        try error_message_buf.writer(self.allocator).print("'{}' is provided while expected an integer", .{provided_type});

        self.error_info = .{ .message = error_message_buf.items, .source_loc = source_loc };

        return error.MismatchedTypes;
    }
}

fn checkCanBeCompared(self: *Sema, provided_type: Type, source_loc: Ast.SourceLoc) Error!void {
    if (provided_type.getPointer()) |pointer| {
        if (pointer.size == .many) {
            var error_message_buf: std.ArrayListUnmanaged(u8) = .{};

            try error_message_buf.writer(self.allocator).print("'{}' cannot be compared", .{provided_type});

            self.error_info = .{ .message = error_message_buf.items, .source_loc = source_loc };

            return error.MismatchedTypes;
        }
    }
}

fn reportIncompatibleTypes(self: *Sema, lhs: Type, rhs: Type, source_loc: Ast.SourceLoc) Error!void {
    var error_message_buf: std.ArrayListUnmanaged(u8) = .{};

    try error_message_buf.writer(self.allocator).print("'{}' is not compatible with '{}'", .{ lhs, rhs });

    self.error_info = .{ .message = error_message_buf.items, .source_loc = source_loc };

    return error.MismatchedTypes;
}

fn reportNotDeclared(self: *Sema, name: Ast.Name) Error!void {
    var error_message_buf: std.ArrayListUnmanaged(u8) = .{};

    try error_message_buf.writer(self.allocator).print("'{s}' is not declared", .{name.buffer});

    self.error_info = .{ .message = error_message_buf.items, .source_loc = name.source_loc };

    return error.Undeclared;
}

fn reportNotPointer(self: *Sema, provided_type: Type, source_loc: Ast.SourceLoc) Error!void {
    var error_message_buf: std.ArrayListUnmanaged(u8) = .{};

    try error_message_buf.writer(self.allocator).print("'{}' is not a pointer", .{provided_type});

    self.error_info = .{ .message = error_message_buf.items, .source_loc = source_loc };

    return error.MismatchedTypes;
}

fn reportNotIndexable(self: *Sema, provided_type: Type, source_loc: Ast.SourceLoc) Error!void {
    var error_message_buf: std.ArrayListUnmanaged(u8) = .{};

    try error_message_buf.writer(self.allocator).print("'{}' does not support indexing", .{provided_type});

    self.error_info = .{ .message = error_message_buf.items, .source_loc = source_loc };

    return error.UnexpectedType;
}

pub fn analyze(self: *Sema, hir: Hir) Error!void {
    const global_scope = try self.scope_stack.addOne(self.allocator);
    global_scope.* = .{};

    self.scope = global_scope;

    try self.scope.put(self.allocator, "true", .{
        .is_const = true,
        .is_comptime = true,
        .symbol = undefined,
        .maybe_value = .{ .boolean = true },
    });

    try self.scope.put(self.allocator, "false", .{
        .is_const = true,
        .is_comptime = true,
        .symbol = undefined,
        .maybe_value = .{ .boolean = false },
    });

    var hir_data_block_iterator = hir.data_blocks.iterator();

    while (hir_data_block_iterator.next()) |hir_block_entry| {
        const hir_block_name = hir_block_entry.key_ptr.*;
        const hir_block = hir_block_entry.value_ptr;

        const lir_block_entry = try self.lir.data_blocks.getOrPutValue(self.allocator, hir_block_name, .{});

        self.lir_block = lir_block_entry.value_ptr;

        for (hir_block.instructions.items) |hir_instruction| {
            try self.analyzeInstruction(hir_instruction);
        }
    }

    var hir_function_iterator = hir.functions.iterator();

    while (hir_function_iterator.next()) |hir_function_entry| {
        const hir_function = hir_function_entry.value_ptr;

        var function_parameter_types: std.ArrayListUnmanaged(Type) = .{};

        for (hir_function.prototype.parameters) |ast_function_parameter| {
            try function_parameter_types.append(self.allocator, ast_function_parameter.expected_type);
        }

        const function_return_type_on_heap = try self.allocator.create(Type);
        function_return_type_on_heap.* = hir_function.prototype.return_type;

        const function_type: Type = .{
            .tag = .function,
            .data = .{
                .function = .{
                    .parameter_types = try function_parameter_types.toOwnedSlice(self.allocator),
                    .return_type = function_return_type_on_heap,
                },
            },
        };

        try self.lir.functions.put(
            self.allocator,
            hir_function_entry.key_ptr.*,
            .{
                .name = hir_function.prototype.name.buffer,
                .type = function_type,
            },
        );

        try self.scope.put(
            self.allocator,
            hir_function.prototype.name.buffer,
            .{
                .symbol = .{
                    .name = hir_function.prototype.name,
                    .type = function_type,
                    .linkage = .global,
                },
                .is_const = true,
            },
        );
    }

    hir_function_iterator.reset();

    while (hir_function_iterator.next()) |hir_function_entry| {
        const hir_function = hir_function_entry.value_ptr;

        const lir_function = self.lir.functions.getPtr(hir_function.prototype.name.buffer).?;

        self.maybe_hir_function = hir_function;

        var hir_block_iterator = hir_function.blocks.iterator();

        while (hir_block_iterator.next()) |hir_block_entry| {
            const hir_block_name = hir_block_entry.key_ptr.*;
            const hir_block = hir_block_entry.value_ptr;

            const lir_block_entry = try lir_function.blocks.getOrPutValue(
                self.allocator,
                hir_block_name,
                .{ .is_control_flow = hir_block.is_control_flow },
            );

            self.lir_block = lir_block_entry.value_ptr;

            const local_scope = try self.scope_stack.addOne(self.allocator);
            local_scope.* = .{ .maybe_parent = self.scope };
            self.scope = local_scope;

            for (hir_block.instructions.items) |hir_instruction| {
                try self.analyzeInstruction(hir_instruction);
            }
        }
    }
}

fn analyzeInstruction(self: *Sema, hir_instruction: Hir.Block.Instruction) Error!void {
    switch (hir_instruction) {
        .parameter => |index| try self.analyzeParameter(index),

        .call => |info| try self.analyzeCall(info),

        .constant => |symbol| try self.analyzeConstant(false, symbol),
        .constant_infer => |symbol| try self.analyzeConstant(true, symbol),

        .variable => |symbol| try self.analyzeVariable(false, symbol),
        .variable_infer => |symbol| try self.analyzeVariable(true, symbol),

        .set => |name| try self.analyzeSet(name),
        .get => |name| try self.analyzeGet(name),

        .string => |string| try self.analyzeString(string),
        .int => |int| try self.analyzeInt(int),
        .float => |float| try self.analyzeFloat(float),

        .negate => |source_loc| try self.analyzeNegate(source_loc),

        .bool_not => |source_loc| try self.analyzeNot(.bool, source_loc),
        .bit_not => |source_loc| try self.analyzeNot(.bit, source_loc),

        .bit_and => |source_loc| try self.analyzeBitwiseArithmetic(.bit_and, source_loc),
        .bit_or => |source_loc| try self.analyzeBitwiseArithmetic(.bit_or, source_loc),
        .bit_xor => |source_loc| try self.analyzeBitwiseArithmetic(.bit_xor, source_loc),

        .reference => |source_loc| try self.analyzeReference(source_loc),

        .read => |source_loc| try self.analyzeRead(source_loc),
        .write => |source_loc| try self.analyzeWrite(source_loc),
        .offset => |source_loc| try self.analyzeOffset(source_loc),

        .add => |source_loc| try self.analyzeArithmetic(.add, source_loc),
        .sub => |source_loc| try self.analyzeArithmetic(.sub, source_loc),
        .mul => |source_loc| try self.analyzeArithmetic(.mul, source_loc),
        .div => |source_loc| try self.analyzeArithmetic(.div, source_loc),

        .lt => |source_loc| try self.analyzeComparison(.lt, source_loc),
        .gt => |source_loc| try self.analyzeComparison(.gt, source_loc),
        .eql => |source_loc| try self.analyzeComparison(.eql, source_loc),

        .shl => |source_loc| try self.analyzeBitwiseShift(.left, source_loc),
        .shr => |source_loc| try self.analyzeBitwiseShift(.right, source_loc),

        .jmp_if_false => |block_name| try self.analyzeJmpIfFalse(block_name),
        .jmp => |block_name| try self.analyzeJmp(block_name),

        .assembly => |assembly| try self.analyzeAssembly(assembly),

        .pop => try self.analyzePop(),

        .@"return" => try self.analyzeReturn(),
    }
}

fn analyzeParameter(self: *Sema, index: usize) Error!void {
    const parameter = self.maybe_hir_function.?.prototype.parameters[index];

    const parameter_symbol: Symbol = .{
        .name = parameter.name,
        .type = parameter.expected_type,
        .linkage = .local,
    };

    try self.scope.put(self.allocator, parameter_symbol.name.buffer, .{ .is_const = true, .symbol = parameter_symbol });

    try self.lir_block.instructions.append(self.allocator, .{ .parameter = .{ index, parameter_symbol } });
}

fn analyzeCall(self: *Sema, info: struct { usize, Ast.SourceLoc }) Error!void {
    const arguments_count, const source_loc = info;

    const callable = self.stack.pop();
    const callable_type = callable.getType();

    var error_message_buf: std.ArrayListUnmanaged(u8) = .{};

    if (callable_type.getFunction()) |function| {
        if (function.parameter_types.len != arguments_count) {
            try error_message_buf.writer(self.allocator).print("expected {} argument(s) got {} argument(s)", .{ function.parameter_types.len, arguments_count });

            self.error_info = .{ .message = error_message_buf.items, .source_loc = source_loc };

            return error.UnexpectedArgumentsCount;
        }

        var i: usize = function.parameter_types.len;

        while (i > 0) {
            i -= 1;

            const parameter_type = function.parameter_types[i];

            const argument = self.stack.pop();

            try self.checkRepresentability(argument, parameter_type, source_loc);
        }

        try self.lir_block.instructions.append(self.allocator, .{ .call = function });

        try self.stack.append(self.allocator, .{ .runtime = .{ .type = function.return_type.*, .data = .none } });
    } else {
        try error_message_buf.writer(self.allocator).print("'{}' is not a callable", .{callable_type});

        self.error_info = .{ .message = error_message_buf.items, .source_loc = source_loc };

        return error.MismatchedTypes;
    }
}

fn analyzeConstant(self: *Sema, infer: bool, symbol: Symbol) Error!void {
    var variable: Variable = .{ .is_const = true, .is_comptime = true, .symbol = symbol };

    const value = self.stack.getLast();

    if (infer) {
        variable.symbol.type = value.getType();
    }

    if (variable.symbol.type.tag == .void) {
        self.error_info = .{ .message = "cannot declare a constant with type 'void'", .source_loc = variable.symbol.name.source_loc };

        return error.UnexpectedType;
    }

    if (value == .runtime) {
        self.error_info = .{ .message = "expected the constant initializer to be compile time known", .source_loc = variable.symbol.name.source_loc };

        return error.ExpectedCompiletimeConstant;
    }

    if (variable.symbol.linkage == .global) {
        var initializer_block_entry = self.lir.data_blocks.pop();
        initializer_block_entry.value.instructions.deinit(self.allocator);
    } else {
        _ = self.lir_block.instructions.pop();
    }

    try self.scope.put(self.allocator, variable.symbol.name.buffer, variable);
}

fn analyzeVariable(self: *Sema, infer: bool, symbol: Symbol) Error!void {
    var variable: Variable = .{ .symbol = symbol };

    if (infer) {
        variable.symbol.type = self.stack.getLast().getType();
    }

    if (variable.symbol.type.tag == .void) {
        self.error_info = .{ .message = "cannot declare a variable with type 'void'", .source_loc = variable.symbol.name.source_loc };

        return error.UnexpectedType;
    }

    if (variable.symbol.type.isAmbigiuous()) {
        self.error_info = .{ .message = "cannot declare a variable with an ambigiuous type", .source_loc = variable.symbol.name.source_loc };

        return error.UnexpectedType;
    }

    try self.scope.put(self.allocator, variable.symbol.name.buffer, variable);

    try self.lir_block.instructions.append(self.allocator, .{ .variable = variable.symbol });
}

fn analyzeSet(self: *Sema, name: Ast.Name) Error!void {
    const variable = self.scope.getPtr(name.buffer) orelse return self.reportNotDeclared(name);

    const value = self.stack.pop();

    if (variable.is_comptime and variable.maybe_value == null) {
        variable.maybe_value = value;
    } else if (variable.is_const) {
        self.error_info = .{ .message = "cannot mutate the value of a constant", .source_loc = name.source_loc };

        return error.UnexpectedMutation;
    } else if (variable.symbol.linkage == .global and value == .runtime and self.maybe_hir_function == null) {
        self.error_info = .{ .message = "expected global variable initializer to be compile time known", .source_loc = name.source_loc };

        return error.ExpectedCompiletimeConstant;
    } else {
        try self.checkRepresentability(value, variable.symbol.type, name.source_loc);

        if (self.maybe_hir_function != null) {
            try self.lir_block.instructions.append(self.allocator, .{ .set = name.buffer });
        }
    }
}

fn analyzeGet(self: *Sema, name: Ast.Name) Error!void {
    const variable = self.scope.get(name.buffer) orelse return self.reportNotDeclared(name);

    if (variable.maybe_value) |value| {
        switch (value) {
            .string => |value_string| try self.lir_block.instructions.append(self.allocator, .{ .string = value_string }),
            .int => |value_int| try self.lir_block.instructions.append(self.allocator, .{ .int = value_int }),
            .float => |value_float| try self.lir_block.instructions.append(self.allocator, .{ .float = value_float }),
            .boolean => |value_boolean| try self.lir_block.instructions.append(self.allocator, .{ .boolean = value_boolean }),
            .runtime => unreachable,
        }

        try self.stack.append(self.allocator, value);
    } else {
        try self.lir_block.instructions.append(self.allocator, .{ .get = name.buffer });

        try self.stack.append(self.allocator, .{ .runtime = .{ .type = variable.symbol.type, .data = .{ .name = variable.symbol.name } } });
    }
}

fn analyzeString(self: *Sema, string: []const u8) Error!void {
    try self.stack.append(self.allocator, .{ .string = string });

    try self.lir_block.instructions.append(self.allocator, .{ .string = string });
}

fn analyzeInt(self: *Sema, int: i128) Error!void {
    try self.stack.append(self.allocator, .{ .int = int });

    try self.lir_block.instructions.append(self.allocator, .{ .int = int });
}

fn analyzeFloat(self: *Sema, float: f64) Error!void {
    try self.stack.append(self.allocator, .{ .float = float });

    try self.lir_block.instructions.append(self.allocator, .{ .float = float });
}

fn analyzeNegate(self: *Sema, source_loc: Ast.SourceLoc) Error!void {
    const rhs = self.stack.pop();

    if (!rhs.getType().canBeNegative()) {
        var error_message_buf: std.ArrayListUnmanaged(u8) = .{};

        try error_message_buf.writer(self.allocator).print("'{}' cannot be negative", .{rhs.getType()});

        self.error_info = .{ .message = error_message_buf.items, .source_loc = source_loc };

        return error.MismatchedTypes;
    }

    switch (rhs) {
        .int => |rhs_int| {
            self.lir_block.instructions.items[self.lir_block.instructions.items.len - 1] = .{ .int = -rhs_int };

            try self.stack.append(self.allocator, .{ .int = -rhs_int });
        },

        .float => |rhs_float| {
            self.lir_block.instructions.items[self.lir_block.instructions.items.len - 1] = .{ .float = -rhs_float };

            try self.stack.append(self.allocator, .{ .float = -rhs_float });
        },

        .runtime => |rhs_runtime| {
            try self.lir_block.instructions.append(self.allocator, .negate);

            try self.stack.append(self.allocator, .{ .runtime = .{ .type = rhs_runtime.type } });
        },

        else => unreachable,
    }
}

const NotOperation = enum {
    bool,
    bit,
};

fn analyzeNot(self: *Sema, comptime operand: NotOperation, source_loc: Ast.SourceLoc) Error!void {
    const rhs = self.stack.pop();
    const rhs_type = rhs.getType();

    if (operand == .bool) {
        try self.checkRepresentability(rhs, .{ .tag = .bool }, source_loc);
    } else if (operand == .bit) {
        try self.checkInt(rhs_type, source_loc);
    }

    switch (rhs) {
        .int => |rhs_int| {
            self.lir_block.instructions.items[self.lir_block.instructions.items.len - 1] = .{ .int = ~rhs_int };

            try self.stack.append(self.allocator, .{ .int = ~rhs_int });
        },

        .boolean => |rhs_boolean| {
            self.lir_block.instructions.items[self.lir_block.instructions.items.len - 1] = .{ .boolean = !rhs_boolean };

            try self.stack.append(self.allocator, .{ .boolean = !rhs_boolean });
        },

        .runtime => |rhs_runtime| {
            try self.lir_block.instructions.append(self.allocator, if (operand == .bool) .bool_not else .bit_not);

            try self.stack.append(self.allocator, .{ .runtime = .{ .type = rhs_runtime.type } });
        },

        else => unreachable,
    }
}

fn analyzeReference(self: *Sema, source_loc: Ast.SourceLoc) Error!void {
    const rhs = self.stack.pop();

    if (!(rhs == .runtime and rhs.runtime.data == .name)) {
        var error_message_buf: std.ArrayListUnmanaged(u8) = .{};

        try error_message_buf.writer(self.allocator).print("'{}' value cannot be referenced", .{rhs.getType()});

        self.error_info = .{ .message = error_message_buf.items, .source_loc = source_loc };

        return error.MismatchedTypes;
    }

    const rhs_runtime_name = rhs.runtime.data.name;
    const rhs_runtime_type = rhs.runtime.type;

    const rhs_variable = self.scope.get(rhs_runtime_name.buffer).?;
    const rhs_symbol = rhs_variable.symbol;

    const child_on_heap = try self.allocator.create(Type);
    child_on_heap.* = rhs_runtime_type;

    self.lir_block.instructions.items[self.lir_block.instructions.items.len - 1] = .{ .get_ptr = rhs_runtime_name.buffer };

    try self.stack.append(self.allocator, .{
        .runtime = .{
            .type = .{
                .tag = .pointer,
                .data = .{
                    .pointer = .{
                        .size = .one,
                        .is_const = rhs_variable.is_const,
                        .is_local = rhs_symbol.linkage == .local,
                        .child_type = child_on_heap,
                    },
                },
            },
        },
    });
}

fn analyzeRead(self: *Sema, source_loc: Ast.SourceLoc) Error!void {
    const rhs = self.stack.pop();
    const rhs_type = rhs.getType();

    const rhs_pointer = rhs_type.getPointer() orelse return self.reportNotPointer(rhs_type, source_loc);

    const result_type = rhs_pointer.child_type.*;

    try self.lir_block.instructions.append(self.allocator, .{ .read = result_type });

    try self.stack.append(self.allocator, .{ .runtime = .{ .type = result_type } });
}

fn analyzeWrite(self: *Sema, source_loc: Ast.SourceLoc) Error!void {
    const rhs = self.stack.pop();

    const lhs = self.stack.pop();
    const lhs_type = lhs.getType();

    const lhs_pointer = lhs_type.getPointer() orelse return self.reportNotPointer(lhs_type, source_loc);

    if (lhs_pointer.is_const) {
        self.error_info = .{ .message = "cannot mutate data pointed by this pointer, it points to read-only data", .source_loc = source_loc };

        return error.UnexpectedMutation;
    }

    try self.checkRepresentability(rhs, lhs_pointer.child_type.*, source_loc);

    try self.lir_block.instructions.append(self.allocator, .write);
}

fn analyzeOffset(self: *Sema, source_loc: Ast.SourceLoc) Error!void {
    const rhs = self.stack.pop();

    const lhs = self.stack.pop();
    const lhs_type = lhs.getType();

    const lhs_pointer = lhs_type.getPointer() orelse return self.reportNotIndexable(lhs_type, source_loc);

    if (lhs_pointer.size != .many) return try self.reportNotIndexable(lhs_type, source_loc);

    const usize_type = Type.makeInt(false, self.env.target.ptrBitWidth());

    try self.checkRepresentability(rhs, usize_type, source_loc);

    try self.lir_block.instructions.append(self.allocator, .{ .int = lhs_pointer.child_type.byteSize(self.env) });
    try self.lir_block.instructions.append(self.allocator, .mul);

    try self.lir_block.instructions.append(self.allocator, .add);

    try self.stack.append(self.allocator, .{ .runtime = .{ .type = lhs_type } });
}

const ComparisonOperation = enum {
    lt,
    gt,
    eql,
};

fn analyzeComparison(self: *Sema, comptime operation: ComparisonOperation, source_loc: Ast.SourceLoc) Error!void {
    const rhs = self.stack.pop();
    const lhs = self.stack.pop();

    const lhs_type = lhs.getType();
    const rhs_type = rhs.getType();

    if (operation == .lt or operation == .gt) {
        try self.checkIntOrFloat(lhs_type, source_loc);
        try self.checkIntOrFloat(rhs_type, source_loc);
    }

    try self.checkCanBeCompared(lhs_type, source_loc);
    try self.checkCanBeCompared(rhs_type, source_loc);

    try self.checkRepresentability(lhs, rhs_type, source_loc);
    try self.checkRepresentability(rhs, lhs_type, source_loc);

    switch (lhs) {
        .int => |lhs_int| switch (rhs) {
            .int => |rhs_int| {
                const result = switch (operation) {
                    .lt => lhs_int < rhs_int,
                    .gt => lhs_int > rhs_int,
                    .eql => lhs_int == rhs_int,
                };

                _ = self.lir_block.instructions.pop();

                self.lir_block.instructions.items[self.lir_block.instructions.items.len - 1] = .{ .boolean = result };

                try self.stack.append(self.allocator, .{ .boolean = result });

                return;
            },

            else => {},
        },

        .float => |lhs_float| switch (rhs) {
            .float => |rhs_float| {
                const result = switch (operation) {
                    .lt => lhs_float < rhs_float,
                    .gt => lhs_float > rhs_float,
                    .eql => lhs_float == rhs_float,
                };

                _ = self.lir_block.instructions.pop();

                self.lir_block.instructions.items[self.lir_block.instructions.items.len - 1] = .{ .boolean = result };

                try self.stack.append(self.allocator, .{ .boolean = result });

                return;
            },

            else => {},
        },

        .boolean => |lhs_boolean| switch (rhs) {
            .boolean => |rhs_boolean| {
                const result = switch (operation) {
                    .eql => lhs_boolean == rhs_boolean,

                    else => unreachable,
                };

                _ = self.lir_block.instructions.pop();

                self.lir_block.instructions.items[self.lir_block.instructions.items.len - 1] = .{ .boolean = result };

                try self.stack.append(self.allocator, .{ .boolean = result });

                return;
            },

            else => {},
        },

        else => {},
    }

    switch (operation) {
        .lt => try self.lir_block.instructions.append(self.allocator, .lt),
        .gt => try self.lir_block.instructions.append(self.allocator, .gt),
        .eql => try self.lir_block.instructions.append(self.allocator, .eql),
    }

    try self.stack.append(self.allocator, .{ .runtime = .{ .type = .{ .tag = .bool } } });
}

const BitwiseShiftDirection = enum {
    left,
    right,
};

fn analyzeBitwiseShift(self: *Sema, comptime direction: BitwiseShiftDirection, source_loc: Ast.SourceLoc) Error!void {
    const rhs = self.stack.pop();
    const lhs = self.stack.pop();

    const lhs_type = lhs.getType();

    try self.checkInt(lhs_type, source_loc);
    try self.checkRepresentability(rhs, .{ .tag = .u8 }, source_loc);

    if (lhs != .runtime and rhs != .runtime) {
        const lhs_int = lhs.int;
        const rhs_int = rhs.int;

        if (rhs_int > std.math.maxInt(u7)) {
            self.error_info = .{
                .message = "cannot bit shift with a count more than '" ++ std.fmt.comptimePrint("{}", .{std.math.maxInt(u7)}) ++ "'",
                .source_loc = source_loc,
            };

            return error.TypeCannotRepresentValue;
        }

        const result = switch (direction) {
            .left => lhs_int << @intCast(rhs_int),
            .right => lhs_int >> @intCast(rhs_int),
        };

        try self.stack.append(self.allocator, .{ .int = result });

        _ = self.lir_block.instructions.pop();

        self.lir_block.instructions.items[self.lir_block.instructions.items.len - 1] = .{ .int = result };
    } else {
        switch (direction) {
            .left => try self.lir_block.instructions.append(self.allocator, .shl),
            .right => try self.lir_block.instructions.append(self.allocator, .shr),
        }

        try self.stack.append(self.allocator, .{ .runtime = .{ .type = lhs_type } });
    }
}

const BitwiseArithmeticOperation = enum {
    bit_and,
    bit_or,
    bit_xor,
};

fn analyzeBitwiseArithmetic(self: *Sema, comptime operation: BitwiseArithmeticOperation, source_loc: Ast.SourceLoc) Error!void {
    const rhs = self.stack.pop();
    const lhs = self.stack.pop();

    const lhs_type = lhs.getType();
    const rhs_type = rhs.getType();

    try self.checkInt(lhs_type, source_loc);
    try self.checkInt(rhs_type, source_loc);

    if (lhs_type.tag != rhs_type.tag and !lhs_type.isAmbigiuous() and !rhs_type.isAmbigiuous()) {
        return self.reportIncompatibleTypes(lhs_type, rhs_type, source_loc);
    }

    switch (operation) {
        .bit_and => try self.lir_block.instructions.append(self.allocator, .bit_and),
        .bit_or => try self.lir_block.instructions.append(self.allocator, .bit_or),
        .bit_xor => try self.lir_block.instructions.append(self.allocator, .bit_xor),
    }

    switch (lhs) {
        .int => |lhs_int| switch (rhs) {
            .int => |rhs_int| {
                const result = switch (operation) {
                    .bit_and => lhs_int & rhs_int,
                    .bit_or => lhs_int | rhs_int,
                    .bit_xor => lhs_int ^ rhs_int,
                };

                try self.stack.append(self.allocator, .{ .int = result });

                _ = self.lir_block.instructions.pop();

                self.lir_block.instructions.items[self.lir_block.instructions.items.len - 1] = .{ .int = result };

                return;
            },

            else => {},
        },

        else => {},
    }

    if (!lhs_type.isAmbigiuous()) {
        // Check if we can represent the rhs ambigiuous value as lhs type (e.g. x + 4)
        if (rhs_type.isAmbigiuous()) {
            try self.checkRepresentability(rhs, lhs_type, source_loc);
        }

        try self.stack.append(self.allocator, .{ .runtime = .{ .type = lhs_type } });
    } else {
        // Check if we can represent the lhs ambigiuous value as rhs type (e.g. 4 + x)
        if (!rhs_type.isAmbigiuous()) {
            try self.checkRepresentability(lhs, rhs_type, source_loc);
        }

        try self.stack.append(self.allocator, .{ .runtime = .{ .type = rhs_type } });
    }
}

const ArithmeticOperation = enum {
    add,
    sub,
    mul,
    div,
};

fn analyzeArithmetic(self: *Sema, comptime operation: ArithmeticOperation, source_loc: Ast.SourceLoc) Error!void {
    const rhs = self.stack.pop();
    const lhs = self.stack.pop();

    const lhs_type = lhs.getType();
    const rhs_type = rhs.getType();

    if (operation == .add or operation == .sub) {
        try self.checkIntOrFloatOrPointer(lhs_type, source_loc);
        try self.checkIntOrFloatOrPointer(rhs_type, source_loc);

        const usize_type = Type.makeInt(false, self.env.target.ptrBitWidth());

        if (lhs_type.tag == .pointer) {
            try self.checkRepresentability(rhs, usize_type, source_loc);
        } else if (rhs_type.tag == .pointer) {
            try self.checkRepresentability(lhs, usize_type, source_loc);
        } else if ((lhs_type.isInt() != rhs_type.isInt()) or
            (lhs_type.tag != rhs_type.tag and !lhs_type.isAmbigiuous() and !rhs_type.isAmbigiuous()))
        {
            return self.reportIncompatibleTypes(lhs_type, rhs_type, source_loc);
        }
    } else {
        try self.checkIntOrFloat(lhs_type, source_loc);
        try self.checkIntOrFloat(rhs_type, source_loc);

        if ((lhs_type.isInt() != rhs_type.isInt()) or
            (lhs_type.tag != rhs_type.tag and !lhs_type.isAmbigiuous() and !rhs_type.isAmbigiuous()))
        {
            return self.reportIncompatibleTypes(lhs_type, rhs_type, source_loc);
        }
    }

    switch (lhs) {
        .int => |lhs_int| switch (rhs) {
            .int => |rhs_int| {
                const result = switch (operation) {
                    .add => lhs_int + rhs_int,
                    .sub => lhs_int - rhs_int,
                    .mul => lhs_int * rhs_int,
                    .div => @divFloor(lhs_int, rhs_int),
                };

                try self.stack.append(self.allocator, .{ .int = result });

                _ = self.lir_block.instructions.pop();

                self.lir_block.instructions.items[self.lir_block.instructions.items.len - 1] = .{ .int = result };

                return;
            },

            else => {},
        },

        .float => |lhs_float| switch (rhs) {
            .float => |rhs_float| {
                const result = switch (operation) {
                    .add => lhs_float + rhs_float,
                    .sub => lhs_float - rhs_float,
                    .mul => lhs_float * rhs_float,
                    .div => lhs_float * rhs_float,
                };

                try self.stack.append(self.allocator, .{ .float = result });

                _ = self.lir_block.instructions.pop();

                self.lir_block.instructions.items[self.lir_block.instructions.items.len - 1] = .{ .float = result };

                return;
            },

            else => {},
        },

        else => {},
    }

    switch (operation) {
        .add => try self.lir_block.instructions.append(self.allocator, .add),
        .sub => try self.lir_block.instructions.append(self.allocator, .sub),
        .mul => try self.lir_block.instructions.append(self.allocator, .mul),
        .div => try self.lir_block.instructions.append(self.allocator, .div),
    }

    if (lhs_type.tag == .pointer) {
        try self.stack.append(self.allocator, .{ .runtime = .{ .type = lhs_type } });
    } else if (rhs_type.tag == .pointer) {
        try self.stack.append(self.allocator, .{ .runtime = .{ .type = rhs_type } });
    } else if (!lhs_type.isAmbigiuous()) {
        // Check if we can represent the rhs ambigiuous value as lhs type (e.g. x + 4)
        if (rhs_type.isAmbigiuous()) {
            try self.checkRepresentability(rhs, lhs_type, source_loc);
        }

        try self.stack.append(self.allocator, .{ .runtime = .{ .type = lhs_type } });
    } else {
        // Check if we can represent the lhs ambigiuous value as rhs type (e.g. 4 + x)
        if (!rhs_type.isAmbigiuous()) {
            try self.checkRepresentability(lhs, rhs_type, source_loc);
        }

        try self.stack.append(self.allocator, .{ .runtime = .{ .type = rhs_type } });
    }
}

fn analyzeJmpIfFalse(self: *Sema, block_name: Ast.Name) Error!void {
    const condition = self.stack.pop();

    try self.checkRepresentability(condition, .{ .tag = .bool }, block_name.source_loc);

    switch (condition) {
        .boolean => |condition_boolean| {
            _ = self.lir_block.instructions.pop();

            if (condition_boolean == false) {
                try self.lir_block.instructions.append(self.allocator, .{ .jmp = block_name.buffer });
            }
        },

        else => {
            try self.lir_block.instructions.append(self.allocator, .{ .jmp_if_false = block_name.buffer });
        },
    }
}

fn analyzeJmp(self: *Sema, block_name: []const u8) Error!void {
    try self.lir_block.instructions.append(self.allocator, .{ .jmp = block_name });
}

fn analyzeAssembly(self: *Sema, assembly: Ast.Node.Expr.Assembly) Error!void {
    if (self.maybe_hir_function == null and (assembly.input_constraints.len > 0 or assembly.output_constraint != null)) {
        self.error_info = .{ .message = "global assembly should not contain any input or output constraints", .source_loc = assembly.source_loc };

        return error.UnexpectedAssemblyConstraints;
    }

    var i: usize = assembly.input_constraints.len;

    while (i > 0) {
        i -= 1;

        _ = self.stack.pop();

        const input_constraint = assembly.input_constraints[i];

        try self.lir_block.instructions.append(self.allocator, .{ .assembly_input = input_constraint.register });
    }

    try self.lir_block.instructions.append(self.allocator, .{ .assembly = assembly.content });

    if (assembly.output_constraint) |output_constraint| {
        try self.lir_block.instructions.append(self.allocator, .{ .assembly_output = output_constraint.register });

        try self.stack.append(self.allocator, .{ .runtime = .{ .type = output_constraint.type } });
    } else {
        try self.stack.append(self.allocator, .{ .runtime = .{ .type = .{ .tag = .void } } });
    }
}

fn analyzePop(self: *Sema) Error!void {
    if (self.stack.popOrNull()) |unused_value| {
        if (unused_value.getType().tag != .void) {
            try self.lir_block.instructions.append(self.allocator, .pop);
        }
    }
}

fn analyzeReturn(self: *Sema) Error!void {
    if (self.maybe_hir_function == null) return;

    if (self.stack.popOrNull()) |return_value| {
        try self.checkRepresentability(return_value, self.maybe_hir_function.?.prototype.return_type, self.maybe_hir_function.?.prototype.name.source_loc);
    } else {
        if (self.maybe_hir_function.?.prototype.return_type.tag != .void) {
            self.error_info = .{ .message = "function with non void return type implicitly returns", .source_loc = self.maybe_hir_function.?.prototype.name.source_loc };

            return error.ExpectedExplicitReturn;
        }
    }

    if (!self.lir_block.is_control_flow) {
        self.maybe_hir_function = null;
    }

    self.scope.clearAndFree(self.allocator);
    self.scope = self.scope.maybe_parent.?;
    _ = self.scope_stack.pop();

    try self.lir_block.instructions.append(self.allocator, .@"return");
}
