//! Semantic Analyzer.
//!
//! An analyzer that lowers down `Hir` to `Lir` while checking if the sematics are completely right.

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

symbol_table: Symbol.Table,

error_info: ?ErrorInfo = null,

pub const ErrorInfo = struct {
    message: []const u8,
    source_loc: Ast.SourceLoc,
};

pub const Error = error{
    ExpectedExplicitReturn,
    TypeCannotRepresentValue,
    MismatchedTypes,
    Undeclared,
} || std.mem.Allocator.Error;

const Value = union(enum) {
    string: []const u8,
    int: i128,
    float: f64,
    symbol: Symbol,

    fn canImplicitCast(self: Value, to: Type) bool {
        return ((self == .int and to.isInt()) or
            (self == .float and to.isFloat()) or
            (self == .string and to.tag == .string) or
            (self == .symbol and self.symbol.type.tag == to.tag));
    }

    fn canBeRepresented(self: Value, as: Type) bool {
        return ((self == .int and self.int >= as.minInt() and
            self == .int and self.int <= as.maxInt()) or
            (self == .float and self.float >= as.minFloat() and
            self == .float and self.float <= as.maxFloat()) or
            (self == .string) or
            (self == .symbol));
    }

    fn getType(self: Value) Type {
        return switch (self) {
            .string => Type{ .tag = .string },
            .int => Type{ .tag = .ambigiuous_int },
            .float => Type{ .tag = .ambigiuous_float },
            .symbol => |symbol| symbol.type,
        };
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
    if (self.function == null and instruction != .label and instruction != .function_proluge) {
        return;
    }

    switch (instruction) {
        .label => |label| try self.hirLabel(label),

        .function_proluge => |function| try self.hirFunctionProluge(function),
        .function_epilogue => try self.hirFunctionEpilogue(),

        .declare => |declare| try self.hirDeclare(declare),

        .set => |name| try self.hirSet(name),
        .get => |name| try self.hirGet(name),

        .string => |string| try self.hirString(string),
        .int => |int| try self.hirInt(int),
        .float => |float| try self.hirFloat(float),

        .negate => |source_loc| try self.hirNegate(source_loc),

        .add => |source_loc| try self.hirBinaryOperation(.plus, source_loc),
        .sub => |source_loc| try self.hirBinaryOperation(.minus, source_loc),
        .mul => |source_loc| try self.hirBinaryOperation(.star, source_loc),
        .div => |source_loc| try self.hirBinaryOperation(.forward_slash, source_loc),

        .pop => try self.hirPop(),

        .assembly => |assembly| try self.hirAssembly(assembly),

        .@"return" => try self.hirReturn(),
    }
}

fn hirLabel(self: *Sema, label: []const u8) Error!void {
    try self.lir.instructions.append(self.allocator, .{ .label = label });
}

fn hirFunctionProluge(self: *Sema, function: Ast.Node.Stmt.FunctionDeclaration) Error!void {
    self.function = function;

    try self.lir.instructions.append(self.allocator, .function_proluge);
}

fn hirFunctionEpilogue(self: *Sema) Error!void {
    try self.lir.instructions.append(self.allocator, .function_epilogue);
}

fn hirDeclare(self: *Sema, declare: Hir.Instruction.Declare) Error!void {
    try self.symbol_table.set(.{
        .name = declare.name,
        .type = declare.type,
    });
}

fn checkRepresentability(self: *Sema, value: Value, intended_type: Type, source_loc: Ast.SourceLoc) Error!void {
    if (!value.canImplicitCast(intended_type)) {
        var error_message_buf: std.ArrayListUnmanaged(u8) = .{};

        try error_message_buf.writer(self.allocator).print("'{s}' cannot be implicitly casted to '{s}'", .{ value.getType().toString(), self.function.?.prototype.return_type.toString() });

        self.error_info = .{ .message = error_message_buf.items, .source_loc = source_loc };

        return error.MismatchedTypes;
    }

    if (!value.canBeRepresented(intended_type)) {
        var error_message_buf: std.ArrayListUnmanaged(u8) = .{};

        try error_message_buf.writer(self.allocator).print("'{s}' cannot represent value '{}'", .{ intended_type.toString(), value.int });

        self.error_info = .{ .message = error_message_buf.items, .source_loc = source_loc };

        return error.TypeCannotRepresentValue;
    }
}

fn hirSet(self: *Sema, name: Ast.Name) Error!void {
    const symbol = self.symbol_table.lookup(name.buffer) catch |err| switch (err) {
        error.Undeclared => {
            var error_message_buf: std.ArrayListUnmanaged(u8) = .{};

            try error_message_buf.writer(self.allocator).print("{s} is not declared", .{name.buffer});

            self.error_info = .{ .message = error_message_buf.items, .source_loc = name.source_loc };

            return error.Undeclared;
        },
    };

    const value = self.stack.pop();

    try self.checkRepresentability(value, symbol.type, name.source_loc);

    try self.lir.instructions.append(self.allocator, .{ .set = name.buffer });
}

fn hirGet(self: *Sema, name: Ast.Name) Error!void {
    const symbol = self.symbol_table.lookup(name.buffer) catch |err| switch (err) {
        error.Undeclared => {
            var error_message_buf: std.ArrayListUnmanaged(u8) = .{};

            try error_message_buf.writer(self.allocator).print("{s} is not declared", .{name.buffer});

            self.error_info = .{ .message = error_message_buf.items, .source_loc = name.source_loc };

            return error.Undeclared;
        },
    };

    try self.stack.append(self.allocator, .{ .symbol = symbol });

    try self.lir.instructions.append(self.allocator, .{ .get = name.buffer });
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

fn hirNegate(self: *Sema, source_loc: Ast.SourceLoc) Error!void {
    const rhs = self.stack.pop();

    if (!rhs.getType().canBeNegative()) {
        var error_message_buf: std.ArrayListUnmanaged(u8) = .{};

        try error_message_buf.writer(self.allocator).print("'{s}' cannot be negative", .{rhs.getType().toString()});

        self.error_info = .{ .message = error_message_buf.items, .source_loc = source_loc };

        return error.MismatchedTypes;
    }

    switch (rhs) {
        .int => |rhs_int| {
            try self.stack.append(self.allocator, .{ .int = -rhs_int });

            self.lir.instructions.items[self.lir.instructions.items.len - 1] = .{ .int = -rhs_int };
        },

        .float => |rhs_float| {
            try self.stack.append(self.allocator, .{ .float = -rhs_float });

            self.lir.instructions.items[self.lir.instructions.items.len - 1] = .{ .float = -rhs_float };
        },

        else => {
            try self.stack.append(self.allocator, rhs);

            try self.lir.instructions.append(self.allocator, .negate);
        },
    }
}

fn checkIntOrFloat(self: *Sema, provided_type: Type, source_loc: Ast.SourceLoc) Error!void {
    if (!provided_type.isIntOrFloat()) {
        var error_message_buf: std.ArrayListUnmanaged(u8) = .{};

        try error_message_buf.writer(self.allocator).print("'{s}' is not an integer nor float", .{provided_type.toString()});

        self.error_info = .{ .message = error_message_buf.items, .source_loc = source_loc };

        return error.MismatchedTypes;
    }
}

fn reportIncompatibleTypes(self: *Sema, lhs: Type, rhs: Type, source_loc: Ast.SourceLoc) Error!void {
    var error_message_buf: std.ArrayListUnmanaged(u8) = .{};

    try error_message_buf.writer(self.allocator).print("'{s}' is not compatible with '{s}'", .{ lhs.toString(), rhs.toString() });

    self.error_info = .{ .message = error_message_buf.items, .source_loc = source_loc };

    return error.MismatchedTypes;
}

const BinaryOperator = enum {
    plus,
    minus,
    star,
    forward_slash,
};

fn hirBinaryOperation(self: *Sema, operator: BinaryOperator, source_loc: Ast.SourceLoc) Error!void {
    const rhs = self.stack.pop();
    const lhs = self.stack.pop();

    const lhs_type = lhs.getType();
    const rhs_type = rhs.getType();

    try self.checkIntOrFloat(lhs_type, source_loc);
    try self.checkIntOrFloat(rhs_type, source_loc);

    if (lhs_type.isInt() != rhs_type.isInt() or (lhs_type.tag != rhs_type.tag and !lhs_type.isAmbigiuous() and !rhs_type.isAmbigiuous())) {
        try self.reportIncompatibleTypes(lhs_type, rhs_type, source_loc);
    }

    switch (lhs) {
        .int => |lhs_int| switch (rhs) {
            .int => |rhs_int| {
                const result = switch (operator) {
                    .plus => lhs_int + rhs_int,
                    .minus => lhs_int - rhs_int,
                    .star => lhs_int * rhs_int,
                    // TODO: Do we need to do it like Zig? using built in functions I mean
                    .forward_slash => @divFloor(lhs_int, rhs_int),
                };

                try self.stack.append(self.allocator, .{ .int = result });

                _ = self.lir.instructions.pop();

                self.lir.instructions.items[self.lir.instructions.items.len - 1] = .{ .int = result };

                return;
            },

            else => {},
        },

        .float => |lhs_float| switch (rhs) {
            .float => |rhs_float| {
                const result = switch (operator) {
                    .plus => lhs_float + rhs_float,
                    .minus => lhs_float - rhs_float,
                    .star => lhs_float * rhs_float,
                    .forward_slash => lhs_float * rhs_float,
                };

                try self.stack.append(self.allocator, .{ .float = result });

                _ = self.lir.instructions.pop();

                self.lir.instructions.items[self.lir.instructions.items.len - 1] = .{ .float = result };

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
    }

    if (!lhs_type.isAmbigiuous()) {
        // Check if we can represent the rhs ambigiuous value as lhs type (e.g. x + 4)
        if (rhs_type.isAmbigiuous()) {
            try self.checkRepresentability(rhs, lhs_type, source_loc);
        }

        try self.stack.append(self.allocator, lhs);
    } else {
        // Check if we can represent the lhs ambigiuous value as rhs type (e.g. 4 + x)
        if (!rhs_type.isAmbigiuous()) {
            try self.checkRepresentability(lhs, rhs_type, source_loc);
        }

        try self.stack.append(self.allocator, rhs);
    }
}

fn hirPop(self: *Sema) Error!void {
    _ = self.stack.pop();

    try self.lir.instructions.append(self.allocator, .pop);
}

fn hirAssembly(self: *Sema, assembly: []const u8) Error!void {
    try self.lir.instructions.append(self.allocator, .{ .assembly = assembly });
}

fn hirReturn(self: *Sema) Error!void {
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
    self.symbol_table.reset();

    try self.lir.instructions.append(self.allocator, .@"return");
}
