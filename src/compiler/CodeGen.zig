const std = @import("std");
const ast = @import("ast.zig");
const Type = @import("type.zig").Type;
const IR = @import("IR.zig");
const CodeGen = @This();

instructions: std.ArrayList(IR.Instruction),
string_literals: std.ArrayList([]const u8),

function: ?ast.Declaration.Function = null,
function_returned: bool = false,

variables: std.StringHashMap(VariableInfo),

error_info: ?ErrorInfo = null,

gpa: std.mem.Allocator,

const VariableInfo = struct { type: Type };

pub const Error = error{ MismatchedTypes, ExpectedReturn, UnexpectedReturn, UndeclaredVariable, Redeclaration } || std.mem.Allocator.Error;

pub const ErrorInfo = struct {
    message: []const u8,
    loc: ast.Loc,
};

pub fn init(gpa: std.mem.Allocator) CodeGen {
    return CodeGen{
        .instructions = std.ArrayList(IR.Instruction).init(gpa),
        .string_literals = std.ArrayList([]const u8).init(gpa),
        .variables = std.StringHashMap(VariableInfo).init(gpa),
        .gpa = gpa,
    };
}

pub fn gen(self: *CodeGen, root: ast.Root) Error!IR {
    for (root.declarations) |declaration| {
        try self.handleDeclaration(declaration);
    }

    return IR{
        .instructions = try self.instructions.toOwnedSlice(),
        .string_literals = try self.string_literals.toOwnedSlice(),
    };
}

fn handleDeclaration(self: *CodeGen, declaration: ast.Declaration) Error!void {
    switch (declaration) {
        .function => try self.handleFunctionDeclaration(declaration.function),
    }
}

fn handleFunctionDeclaration(self: *CodeGen, function: ast.Declaration.Function) Error!void {
    try self.instructions.append(.{ .label = .{ .name = function.prototype.name.buffer } });

    self.function = function;

    for (self.function.?.body) |node| {
        try self.handleNode(node);
    }

    if (!self.function_returned) {
        if (self.function.?.prototype.return_type == .void_type) {
            try self.instructions.append(.{ .ret = .{} });
        } else {
            self.error_info = .{ .message = "expected function with non-void return type to explicitly return", .loc = self.function.?.prototype.name.loc };

            return error.ExpectedReturn;
        }
    }

    self.variables.clearAndFree();

    self.function = null;
    self.function_returned = false;
}

fn handleNode(self: *CodeGen, node: ast.Node) Error!void {
    switch (node) {
        .stmt => try self.handleStmt(node.stmt),
        .expr => {},
    }
}

fn handleStmt(self: *CodeGen, stmt: ast.Node.Stmt) Error!void {
    switch (stmt) {
        .variable_declaration => try self.handleVariableDeclarationStmt(stmt.variable_declaration),

        .ret => try self.handleReturnStmt(stmt.ret),
    }
}

fn handleVariableDeclarationStmt(self: *CodeGen, variable: ast.Node.Stmt.VariableDeclaration) Error!void {
    const value = try self.genValue(variable.value);

    if (variable.type != self.inferType(variable.value)) {
        var buf = std.ArrayList(u8).init(self.gpa);

        try buf.writer().print("expected type '{s}' got '{s}'", .{ variable.type.to_string(), self.inferType(variable.value).to_string() });

        self.error_info = .{ .message = try buf.toOwnedSlice(), .loc = variable.name.loc };

        return error.MismatchedTypes;
    }

    if (self.variables.contains(variable.name.buffer)) {
        var buf = std.ArrayList(u8).init(self.gpa);

        try buf.writer().print("redeclaration of {s}", .{variable.name.buffer});

        self.error_info = .{ .message = try buf.toOwnedSlice(), .loc = variable.name.loc };

        return error.Redeclaration;
    }

    try self.variables.put(variable.name.buffer, .{ .type = variable.type });

    try self.instructions.append(.{ .store = .{ .name = variable.name.buffer, .value = value } });
}

fn handleReturnStmt(self: *CodeGen, ret: ast.Node.Stmt.Return) Error!void {
    const value = try self.genValue(ret.value);

    if (self.function.?.prototype.return_type == .void_type) {
        self.error_info = .{ .message = "didn't expect function with void return type to explicitly return", .loc = ret.loc };

        return error.UnexpectedReturn;
    }

    if (self.function.?.prototype.return_type != self.inferType(ret.value)) {
        var buf = std.ArrayList(u8).init(self.gpa);

        try buf.writer().print("expected return type '{s}' got '{s}'", .{ self.function.?.prototype.return_type.to_string(), self.inferType(ret.value).to_string() });

        self.error_info = .{ .message = try buf.toOwnedSlice(), .loc = ret.loc };

        return error.MismatchedTypes;
    }

    self.function_returned = true;

    try self.instructions.append(.{ .load = .{ .value = value } });

    try self.instructions.append(.{ .ret = .{} });
}

fn genValue(self: *CodeGen, expr: ast.Node.Expr) Error!IR.Value {
    return switch (expr) {
        .identifier => {
            if (!self.variables.contains(expr.identifier.symbol.buffer)) {
                var buf = std.ArrayList(u8).init(self.gpa);

                try buf.writer().print("{s} is not declared", .{expr.identifier.symbol.buffer});

                self.error_info = .{ .message = try buf.toOwnedSlice(), .loc = expr.identifier.symbol.loc };

                return error.UndeclaredVariable;
            }

            return .{ .variable_reference = .{ .name = expr.identifier.symbol.buffer } };
        },

        .string => {
            try self.string_literals.append(expr.string.value);

            const index = self.string_literals.items.len - 1;

            return .{ .string_reference = .{ .index = index } };
        },

        .char => .{ .char = .{ .value = expr.char.value } },
        .int => .{ .int = .{ .value = expr.int.value } },
        .float => .{ .float = .{ .value = expr.float.value } },
    };
}

fn inferType(self: CodeGen, expr: ast.Node.Expr) Type {
    return switch (expr) {
        .identifier => {
            const variable_info = self.variables.get(expr.identifier.symbol.buffer).?;

            return variable_info.type;
        },

        .string => .string_type,
        .char => .char_type,
        .int => .int_type,
        .float => .float_type,
    };
}

test "generating function declaration intermediate representation" {
    try testGen(
        \\fn main() int {
        \\  return 0
        \\}
    , .{ .instructions = &.{ .{ .label = .{ .name = "main" } }, .{ .ret = .{ .value = .{ .int = .{ .value = 0 } } } } }, .string_literals = &.{} });
}

fn testGen(source: [:0]const u8, expected_ir: IR) !void {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();

    const allocator = arena.allocator();

    var parser = try ast.Parser.init(source, allocator);

    const root = try parser.parseRoot();

    var codegen = CodeGen.init(allocator);

    const ir = try codegen.gen(root);

    try std.testing.expectEqualDeep(expected_ir, ir);
}
