//! Semantic Analyzer.
//!
//! An analyzer that lowers down `Hir` to `Lir` while checking if the semantics are completely right.

const std = @import("std");

const Ast = @import("Ast.zig");
const Hir = @import("Hir.zig");
const Lir = @import("Lir.zig");
const Symbol = @import("Symbol.zig");
const Type = @import("Type.zig");

const Sema = @This();

allocator: std.mem.Allocator,

lir: Lir = .{},

stack: std.ArrayListUnmanaged(Value) = .{},

function: ?Ast.Node.Stmt.FunctionDeclaration = null,
function_parameter_index: usize = 0,

symbol_table: Symbol.Table,

error_info: ?ErrorInfo = null,

pub const ErrorInfo = struct {
    message: []const u8,
    source_loc: Ast.SourceLoc,
};

pub const Error = error{
    ExpectedCompiletimeConstant,
    UnexpectedArgumentsCount,
    ExpectedExplicitReturn,
    TypeCannotRepresentValue,
    UnexpectedType,
    MismatchedTypes,
    Undeclared,
    Redeclared,
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

pub fn init(allocator: std.mem.Allocator) Sema {
    return Sema{
        .allocator = allocator,
        .symbol_table = Symbol.Table.init(allocator),
    };
}

pub fn analyze(self: *Sema, hir: Hir) Error!void {
    try self.hirInstructions(hir.instructions.items);
}

fn hirInstructions(self: *Sema, instructions: []const Hir.Instruction) Error!void {
    for (instructions) |instruction| {
        try self.hirInstruction(instruction);
    }
}

fn hirInstruction(self: *Sema, instruction: Hir.Instruction) Error!void {
    switch (instruction) {
        .label => |info| try self.hirLabel(info),

        .function_proluge => |function| try self.hirFunctionProluge(function),
        .function_epilogue => try self.hirFunctionEpilogue(),
        .function_parameter => try self.hirFunctionParameter(),

        .call => |info| try self.hirCall(info),

        .variable => |symbol| try self.hirVariable(symbol),
        .variable_infer => |symbol| try self.hirVariableInfer(symbol),

        .set => |name| try self.hirSet(name),
        .get => |name| try self.hirGet(name),

        .string => |string| try self.hirString(string),
        .int => |int| try self.hirInt(int),
        .float => |float| try self.hirFloat(float),
        .boolean => |boolean| try self.hirBoolean(boolean),

        .negate => |source_loc| try self.hirNegate(source_loc),

        .bool_not => |source_loc| try self.hirNot(.bool, source_loc),
        .bit_not => |source_loc| try self.hirNot(.bit, source_loc),

        .reference => |source_loc| try self.hirReference(source_loc),

        .read => |source_loc| try self.hirRead(source_loc),
        .write => |source_loc| try self.hirWrite(source_loc),

        .add => |source_loc| try self.hirBinaryOperation(.plus, source_loc),
        .sub => |source_loc| try self.hirBinaryOperation(.minus, source_loc),
        .mul => |source_loc| try self.hirBinaryOperation(.star, source_loc),
        .div => |source_loc| try self.hirBinaryOperation(.forward_slash, source_loc),
        .lt => |source_loc| try self.hirBinaryOperation(.less_than, source_loc),
        .gt => |source_loc| try self.hirBinaryOperation(.greater_than, source_loc),
        .eql => |source_loc| try self.hirBinaryOperation(.double_equal_sign, source_loc),
        .shl => |source_loc| try self.hirBinaryOperation(.double_less_than, source_loc),
        .shr => |source_loc| try self.hirBinaryOperation(.double_greater_than, source_loc),

        .pop => try self.hirPop(),

        .assembly => |assembly| try self.hirAssembly(assembly),

        .@"return" => try self.hirReturn(),
    }
}

fn hirLabel(self: *Sema, info: struct { bool, Ast.Name }) Error!void {
    const is_function, const name = info;

    if (self.symbol_table.get(name.buffer) != null) {
        return self.reportRedeclaration(name);
    }

    try self.lir.instructions.append(self.allocator, .{ .label = .{ is_function, name.buffer } });
}

fn hirFunctionProluge(self: *Sema, function: Ast.Node.Stmt.FunctionDeclaration) Error!void {
    try self.lir.instructions.append(self.allocator, .function_proluge);

    self.function = function;
    self.function_parameter_index = 0;
}

fn hirFunctionEpilogue(self: *Sema) Error!void {
    if (self.function == null) return;

    try self.lir.instructions.append(self.allocator, .function_epilogue);
}

fn hirFunctionParameter(self: *Sema) Error!void {
    const function_parameter = self.function.?.prototype.parameters[self.function_parameter_index];

    const function_parameter_symbol: Symbol = .{
        .name = function_parameter.name,
        .type = function_parameter.expected_type,
        .linkage = .local,
    };

    try self.symbol_table.put(function_parameter_symbol);

    try self.lir.instructions.append(self.allocator, .{ .function_parameter = .{ self.function_parameter_index, function_parameter_symbol } });

    self.function_parameter_index += 1;
}

fn hirCall(self: *Sema, info: struct { usize, Ast.SourceLoc }) Error!void {
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

        try self.lir.instructions.append(self.allocator, .{ .call = function });

        try self.stack.append(self.allocator, .{ .runtime = .{ .type = function.return_type.*, .data = .none } });
    } else {
        try error_message_buf.writer(self.allocator).print("'{}' is not a callable", .{callable_type});

        self.error_info = .{ .message = error_message_buf.items, .source_loc = source_loc };

        return error.MismatchedTypes;
    }
}

fn hirVariable(self: *Sema, symbol: Symbol) Error!void {
    if (symbol.type.tag == .void) {
        self.error_info = .{ .message = "you cannot declare a variable with type 'void'", .source_loc = symbol.name.source_loc };

        return error.UnexpectedType;
    }

    try self.symbol_table.put(symbol);

    try self.lir.instructions.append(self.allocator, .{ .variable = symbol });
}

fn hirVariableInfer(self: *Sema, symbol: Symbol) Error!void {
    var modified_symbol = symbol;

    modified_symbol.type = self.stack.getLast().getType();

    if (modified_symbol.type.tag == .void) {
        self.error_info = .{ .message = "you cannot declare a variable with type 'void'", .source_loc = modified_symbol.name.source_loc };

        return error.UnexpectedType;
    }

    if (modified_symbol.type.isAmbigiuous()) {
        self.error_info = .{ .message = "you cannot declare a variable with an ambigiuous type", .source_loc = modified_symbol.name.source_loc };

        return error.UnexpectedType;
    }

    try self.symbol_table.put(modified_symbol);

    try self.lir.instructions.append(self.allocator, .{ .variable = modified_symbol });
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

fn reportNotDeclared(self: *Sema, name: Ast.Name) Error!void {
    var error_message_buf: std.ArrayListUnmanaged(u8) = .{};

    try error_message_buf.writer(self.allocator).print("'{s}' is not declared", .{name.buffer});

    self.error_info = .{ .message = error_message_buf.items, .source_loc = name.source_loc };

    return error.Undeclared;
}

fn reportRedeclaration(self: *Sema, name: Ast.Name) Error!void {
    var error_message_buf: std.ArrayListUnmanaged(u8) = .{};

    try error_message_buf.writer(self.allocator).print("redeclaration of '{s}'", .{name.buffer});

    self.error_info = .{ .message = error_message_buf.items, .source_loc = name.source_loc };

    return error.Redeclared;
}

fn hirSet(self: *Sema, name: Ast.Name) Error!void {
    const symbol = self.symbol_table.get(name.buffer) orelse return self.reportNotDeclared(name);

    const value = self.stack.pop();

    if (value == .runtime and symbol.linkage == .global and self.function == null) {
        self.error_info = .{ .message = "expected global variable initializer to be compile time constant", .source_loc = name.source_loc };

        return error.ExpectedCompiletimeConstant;
    }

    try self.checkRepresentability(value, symbol.type, name.source_loc);

    if (self.function != null) {
        try self.lir.instructions.append(self.allocator, .{ .set = name.buffer });
    }
}

fn hirGet(self: *Sema, name: Ast.Name) Error!void {
    const symbol = self.symbol_table.get(name.buffer) orelse return self.reportNotDeclared(name);

    try self.lir.instructions.append(self.allocator, .{ .get = name.buffer });

    try self.stack.append(self.allocator, .{ .runtime = .{ .type = symbol.type, .data = .{ .name = symbol.name } } });
}

fn hirString(self: *Sema, string: []const u8) Error!void {
    try self.stack.append(self.allocator, .{ .string = string });

    try self.lir.instructions.append(self.allocator, .{ .string = string });
}

fn hirInt(self: *Sema, int: i128) Error!void {
    try self.stack.append(self.allocator, .{ .int = int });

    try self.lir.instructions.append(self.allocator, .{ .int = int });
}

fn hirFloat(self: *Sema, float: f64) Error!void {
    try self.stack.append(self.allocator, .{ .float = float });

    try self.lir.instructions.append(self.allocator, .{ .float = float });
}

fn hirBoolean(self: *Sema, boolean: bool) Error!void {
    try self.stack.append(self.allocator, .{ .boolean = boolean });

    try self.lir.instructions.append(self.allocator, .{ .boolean = boolean });
}

fn hirNegate(self: *Sema, source_loc: Ast.SourceLoc) Error!void {
    const rhs = self.stack.pop();

    if (!rhs.getType().canBeNegative()) {
        var error_message_buf: std.ArrayListUnmanaged(u8) = .{};

        try error_message_buf.writer(self.allocator).print("'{}' cannot be negative", .{rhs.getType()});

        self.error_info = .{ .message = error_message_buf.items, .source_loc = source_loc };

        return error.MismatchedTypes;
    }

    switch (rhs) {
        .int => |rhs_int| {
            self.lir.instructions.items[self.lir.instructions.items.len - 1] = .{ .int = -rhs_int };

            try self.stack.append(self.allocator, .{ .int = -rhs_int });
        },

        .float => |rhs_float| {
            self.lir.instructions.items[self.lir.instructions.items.len - 1] = .{ .float = -rhs_float };

            try self.stack.append(self.allocator, .{ .float = -rhs_float });
        },

        .runtime => |rhs_runtime| {
            try self.lir.instructions.append(self.allocator, .negate);

            try self.stack.append(self.allocator, .{ .runtime = .{ .type = rhs_runtime.type } });
        },

        else => unreachable,
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

fn checkBool(self: *Sema, provided_type: Type, source_loc: Ast.SourceLoc) Error!void {
    if (provided_type.tag != .bool) {
        var error_message_buf: std.ArrayListUnmanaged(u8) = .{};

        try error_message_buf.writer(self.allocator).print("'{}' is provided while expected 'bool'", .{provided_type});

        self.error_info = .{ .message = error_message_buf.items, .source_loc = source_loc };

        return error.MismatchedTypes;
    }
}

const NotOperation = enum {
    bool,
    bit,
};

fn hirNot(self: *Sema, comptime operand: NotOperation, source_loc: Ast.SourceLoc) Error!void {
    const rhs = self.stack.pop();
    const rhs_type = rhs.getType();

    if (operand == .bool) {
        try self.checkBool(rhs_type, source_loc);
    } else if (operand == .bit) {
        try self.checkInt(rhs_type, source_loc);
    }

    switch (rhs) {
        .int => |rhs_int| {
            self.lir.instructions.items[self.lir.instructions.items.len - 1] = .{ .int = ~rhs_int };

            try self.stack.append(self.allocator, .{ .int = ~rhs_int });
        },

        .boolean => |rhs_boolean| {
            self.lir.instructions.items[self.lir.instructions.items.len - 1] = .{ .boolean = !rhs_boolean };

            try self.stack.append(self.allocator, .{ .boolean = !rhs_boolean });
        },

        .runtime => |rhs_runtime| {
            try self.lir.instructions.append(self.allocator, if (operand == .bool) .bool_not else .bit_not);

            try self.stack.append(self.allocator, .{ .runtime = .{ .type = rhs_runtime.type } });
        },

        else => unreachable,
    }
}

fn hirReference(self: *Sema, source_loc: Ast.SourceLoc) Error!void {
    const rhs = self.stack.pop();

    if (!(rhs == .runtime and rhs.runtime.data == .name)) {
        var error_message_buf: std.ArrayListUnmanaged(u8) = .{};

        try error_message_buf.writer(self.allocator).print("'{}' value cannot be referenced", .{rhs.getType()});

        self.error_info = .{ .message = error_message_buf.items, .source_loc = source_loc };

        return error.MismatchedTypes;
    }

    const rhs_runtime_name = rhs.runtime.data.name;
    const rhs_runtime_type = rhs.runtime.type;

    const rhs_symbol = self.symbol_table.get(rhs_runtime_name.buffer).?;

    const child_on_heap = try self.allocator.create(Type);
    child_on_heap.* = rhs_runtime_type;

    self.lir.instructions.items[self.lir.instructions.items.len - 1] = .{ .get_ptr = rhs_runtime_name.buffer };

    try self.stack.append(self.allocator, .{
        .runtime = .{
            .type = .{
                .tag = .pointer,
                .data = .{
                    .pointer = .{
                        .size = .one,
                        .is_const = false,
                        .is_local = rhs_symbol.linkage == .local,
                        .child_type = child_on_heap,
                    },
                },
            },
        },
    });
}

fn reportNotPointer(self: *Sema, provided_type: Type, source_loc: Ast.SourceLoc) Error!void {
    var error_message_buf: std.ArrayListUnmanaged(u8) = .{};

    try error_message_buf.writer(self.allocator).print("'{}' is not a pointer", .{provided_type});

    self.error_info = .{ .message = error_message_buf.items, .source_loc = source_loc };

    return error.MismatchedTypes;
}

fn hirRead(self: *Sema, source_loc: Ast.SourceLoc) Error!void {
    const rhs = self.stack.pop();
    const rhs_type = rhs.getType();

    const rhs_pointer = rhs_type.getPointer() orelse return self.reportNotPointer(rhs_type, source_loc);

    const result_type = rhs_pointer.child_type.*;

    try self.lir.instructions.append(self.allocator, .{ .read = result_type });

    try self.stack.append(self.allocator, .{ .runtime = .{ .type = result_type } });
}

fn hirWrite(self: *Sema, source_loc: Ast.SourceLoc) Error!void {
    const rhs = self.stack.pop();

    const lhs = self.stack.pop();
    const lhs_type = lhs.getType();

    const lhs_pointer = lhs_type.getPointer() orelse return self.reportNotPointer(lhs_type, source_loc);

    try self.checkRepresentability(rhs, lhs_pointer.child_type.*, source_loc);

    try self.lir.instructions.append(self.allocator, .write);
}

fn checkIntOrFloat(self: *Sema, provided_type: Type, source_loc: Ast.SourceLoc) Error!void {
    if (!provided_type.isIntOrFloat()) {
        var error_message_buf: std.ArrayListUnmanaged(u8) = .{};

        try error_message_buf.writer(self.allocator).print("'{}' is provided while expected an integer or float", .{provided_type});

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

const BinaryOperator = enum {
    plus,
    minus,
    star,
    forward_slash,
    less_than,
    double_less_than,
    greater_than,
    double_greater_than,
    double_equal_sign,

    fn isComparison(self: BinaryOperator) bool {
        return self == .less_than or self == .greater_than or self == .double_equal_sign;
    }

    fn isBitShift(self: BinaryOperator) bool {
        return self == .double_less_than or self == .double_greater_than;
    }
};

fn hirBinaryOperation(self: *Sema, comptime operator: BinaryOperator, source_loc: Ast.SourceLoc) Error!void {
    const rhs = self.stack.pop();
    const lhs = self.stack.pop();

    const lhs_type = lhs.getType();
    const rhs_type = rhs.getType();

    if (operator == .double_equal_sign) {
        try self.checkCanBeCompared(lhs_type, source_loc);
        try self.checkCanBeCompared(rhs_type, source_loc);
        try self.checkRepresentability(lhs, rhs_type, source_loc);
        try self.checkRepresentability(rhs, lhs_type, source_loc);
    } else if (operator.isBitShift()) {
        try self.checkInt(lhs_type, source_loc);
        try self.checkRepresentability(rhs, .{ .tag = .u8 }, source_loc);
    } else {
        try self.checkIntOrFloat(lhs_type, source_loc);
        try self.checkIntOrFloat(rhs_type, source_loc);
    }

    if (lhs_type.isInt() != rhs_type.isInt() or (lhs_type.tag != rhs_type.tag and !lhs_type.isAmbigiuous() and !rhs_type.isAmbigiuous())) {
        return self.reportIncompatibleTypes(lhs_type, rhs_type, source_loc);
    }

    switch (lhs) {
        .int => |lhs_int| switch (rhs) {
            .int => |rhs_int| {
                if (operator.isComparison()) {
                    const result = switch (operator) {
                        .less_than => lhs_int < rhs_int,
                        .greater_than => lhs_int > rhs_int,
                        .double_equal_sign => lhs_int == rhs_int,
                        else => unreachable,
                    };

                    try self.stack.append(self.allocator, .{ .boolean = result });

                    _ = self.lir.instructions.pop();

                    self.lir.instructions.items[self.lir.instructions.items.len - 1] = .{ .boolean = result };
                } else if (operator.isBitShift()) {
                    if (rhs_int > std.math.maxInt(u7)) {
                        self.error_info = .{
                            .message = "cannot bit shift with a count more than '" ++ std.fmt.comptimePrint("{}", .{std.math.maxInt(u7)}) ++ "'",
                            .source_loc = source_loc,
                        };

                        return error.TypeCannotRepresentValue;
                    }

                    const result = switch (operator) {
                        .double_less_than => lhs_int << @intCast(rhs_int),
                        .double_greater_than => lhs_int >> @intCast(rhs_int),
                        else => unreachable,
                    };

                    try self.stack.append(self.allocator, .{ .int = result });

                    _ = self.lir.instructions.pop();

                    self.lir.instructions.items[self.lir.instructions.items.len - 1] = .{ .int = result };
                } else {
                    const result = switch (operator) {
                        .plus => lhs_int + rhs_int,
                        .minus => lhs_int - rhs_int,
                        .star => lhs_int * rhs_int,
                        .forward_slash => @divFloor(lhs_int, rhs_int),
                        else => unreachable,
                    };

                    try self.stack.append(self.allocator, .{ .int = result });

                    _ = self.lir.instructions.pop();

                    self.lir.instructions.items[self.lir.instructions.items.len - 1] = .{ .int = result };
                }

                return;
            },

            else => {},
        },

        .float => |lhs_float| switch (rhs) {
            .float => |rhs_float| {
                if (operator.isComparison()) {
                    const result = switch (operator) {
                        .less_than => lhs_float < rhs_float,
                        .greater_than => lhs_float > rhs_float,
                        .double_equal_sign => lhs_float == rhs_float,
                        else => unreachable,
                    };

                    try self.stack.append(self.allocator, .{ .boolean = result });

                    _ = self.lir.instructions.pop();

                    self.lir.instructions.items[self.lir.instructions.items.len - 1] = .{ .boolean = result };
                } else {
                    const result = switch (operator) {
                        .plus => lhs_float + rhs_float,
                        .minus => lhs_float - rhs_float,
                        .star => lhs_float * rhs_float,
                        .forward_slash => lhs_float * rhs_float,
                        else => unreachable,
                    };

                    try self.stack.append(self.allocator, .{ .float = result });

                    _ = self.lir.instructions.pop();

                    self.lir.instructions.items[self.lir.instructions.items.len - 1] = .{ .float = result };
                }

                return;
            },

            else => {},
        },

        .boolean => |lhs_boolean| switch (rhs) {
            .boolean => |rhs_boolean| {
                const result = lhs_boolean == rhs_boolean;

                try self.stack.append(self.allocator, .{ .boolean = result });

                _ = self.lir.instructions.pop();

                self.lir.instructions.items[self.lir.instructions.items.len - 1] = .{ .boolean = result };

                return;
            },

            else => {},
        },

        else => {},
    }

    switch (operator) {
        .plus => try self.lir.instructions.append(self.allocator, .add),
        .minus => try self.lir.instructions.append(self.allocator, .sub),
        .star => try self.lir.instructions.append(self.allocator, .mul),
        .forward_slash => try self.lir.instructions.append(self.allocator, .div),
        .less_than => try self.lir.instructions.append(self.allocator, .lt),
        .double_less_than => try self.lir.instructions.append(self.allocator, .shl),
        .greater_than => try self.lir.instructions.append(self.allocator, .gt),
        .double_greater_than => try self.lir.instructions.append(self.allocator, .shr),
        .double_equal_sign => try self.lir.instructions.append(self.allocator, .eql),
    }

    if (operator.isComparison()) {
        try self.stack.append(self.allocator, .{ .runtime = .{ .type = .{ .tag = .bool } } });
    } else if (operator.isBitShift()) {
        try self.stack.append(self.allocator, .{ .runtime = .{ .type = lhs_type } });
    } else {
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
}

fn hirPop(self: *Sema) Error!void {
    if (self.stack.popOrNull()) |unused_value| {
        if (unused_value.getType().tag != .void) {
            try self.lir.instructions.append(self.allocator, .pop);
        }
    }
}

fn hirAssembly(self: *Sema, assembly: Ast.Node.Expr.Assembly) Error!void {
    var i: usize = assembly.input_constraints.len;

    while (i > 0) {
        i -= 1;

        _ = self.stack.pop();

        const input_constraint = assembly.input_constraints[i];

        try self.lir.instructions.append(self.allocator, .{ .assembly_input = input_constraint.register });
    }

    try self.lir.instructions.append(self.allocator, .{ .assembly = assembly.content });

    if (assembly.output_constraint) |output_constraint| {
        try self.lir.instructions.append(self.allocator, .{ .assembly_output = output_constraint.register });

        try self.stack.append(self.allocator, .{ .runtime = .{ .type = output_constraint.type } });
    } else {
        try self.stack.append(self.allocator, .{ .runtime = .{ .type = .{ .tag = .void } } });
    }
}

fn hirReturn(self: *Sema) Error!void {
    if (self.function == null) return;

    if (self.stack.popOrNull()) |return_value| {
        try self.checkRepresentability(return_value, self.function.?.prototype.return_type, self.function.?.prototype.name.source_loc);
    } else {
        if (self.function.?.prototype.return_type.tag != .void) {
            self.error_info = .{ .message = "function with non void return type implicitly returns", .source_loc = self.function.?.prototype.name.source_loc };

            return error.ExpectedExplicitReturn;
        }
    }

    self.function = null;
    self.stack.clearRetainingCapacity();
    self.symbol_table.clearAndFree();

    try self.lir.instructions.append(self.allocator, .@"return");
}
