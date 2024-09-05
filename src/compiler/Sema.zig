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
    int: i64,
    float: f64,
    symbol: Symbol,

    fn canImplicitCast(self: Value, to: Type) bool {
        return ((self == .int and to.isInt()) or (self == .float and to.isFloat()) or (self == .string and to.tag == .string) or (self == .symbol and self.symbol.type.tag == to.tag));
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

    if (!value.canImplicitCast(symbol.type)) {
        var error_message_buf: std.ArrayListUnmanaged(u8) = .{};

        try error_message_buf.writer(self.allocator).print("'{s}' cannot be implicitly casted to '{s}'", .{ value.getType().toString(), self.function.?.prototype.return_type.toString() });

        self.error_info = .{ .message = error_message_buf.items, .source_loc = name.source_loc };

        return error.MismatchedTypes;
    }

    if (value == .int and value.int < 0 and !symbol.type.canBeNegative()) {
        var error_message_buf: std.ArrayListUnmanaged(u8) = .{};

        try error_message_buf.writer(self.allocator).print("'{s}' cannot represent integer value '{}'", .{ symbol.type.toString(), value.int });

        self.error_info = .{ .message = error_message_buf.items, .source_loc = name.source_loc };

        return error.TypeCannotRepresentValue;
    }

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

fn hirInt(self: *Sema, int: i64) Error!void {
    try self.stack.append(self.allocator, .{ .int = int });

    try self.lir.instructions.append(self.allocator, .{ .int = int });
}

fn hirFloat(self: *Sema, float: f64) Error!void {
    try self.stack.append(self.allocator, .{ .float = float });

    try self.lir.instructions.append(self.allocator, .{ .float = float });
}

fn hirNegate(self: *Sema, source_loc: Ast.SourceLoc) Error!void {
    const value = self.stack.pop();

    if (!value.getType().canBeNegative()) {
        var error_message_buf: std.ArrayListUnmanaged(u8) = .{};

        try error_message_buf.writer(self.allocator).print("'{s}' cannot be negative", .{value.getType().toString()});

        self.error_info = .{ .message = error_message_buf.items, .source_loc = source_loc };

        return error.MismatchedTypes;
    }

    switch (value) {
        .int => |int| {
            try self.stack.append(self.allocator, .{ .int = -int });

            self.lir.instructions.items[self.lir.instructions.items.len - 1] = .{ .int = -int };
        },

        .float => |float| {
            const negated_float = -float;

            try self.stack.append(self.allocator, .{ .float = negated_float });

            self.lir.instructions.items[self.lir.instructions.items.len - 1] = .{ .float = negated_float };
        },

        else => {
            try self.stack.append(self.allocator, value);

            try self.lir.instructions.append(self.allocator, .negate);
        },
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
        if (!return_value.canImplicitCast(self.function.?.prototype.return_type)) {
            var error_message_buf: std.ArrayListUnmanaged(u8) = .{};

            try error_message_buf.writer(self.allocator).print("'{s}' cannot be implicitly casted to '{s}'", .{ return_value.getType().toString(), self.function.?.prototype.return_type.toString() });

            self.error_info = .{ .message = error_message_buf.items, .source_loc = self.function.?.prototype.name.source_loc };

            return error.MismatchedTypes;
        }
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
