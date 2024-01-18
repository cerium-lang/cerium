const std = @import("std");
const ast = @import("ast.zig");
const Type = @import("type.zig").Type;
const IR = @import("IR.zig");
const CodeGen = @This();

instructions: std.ArrayList(IR.Instruction),
string_literals: std.ArrayList([]const u8),
function: ?ast.Declaration.Function = null,
error_info: ?ErrorInfo = null,
gpa: std.mem.Allocator,

pub const Error = error{
    MismatchedTypes,
    UnexpectedReturn,
} || std.mem.Allocator.Error;

pub const ErrorInfo = struct {
    message: []const u8,
    loc: ast.Loc,
};

pub fn init(gpa: std.mem.Allocator) CodeGen {
    return CodeGen{
        .instructions = std.ArrayList(IR.Instruction).init(gpa),
        .string_literals = std.ArrayList([]const u8).init(gpa),
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
}

fn handleNode(self: *CodeGen, node: ast.Node) Error!void {
    switch (node) {
        .stmt => try self.handleStmt(node.stmt),
        .expr => {},
    }
}

fn handleStmt(self: *CodeGen, stmt: ast.Node.Stmt) Error!void {
    switch (stmt) {
        .ret => try self.handleReturnStmt(stmt.ret),
    }
}

fn handleReturnStmt(self: *CodeGen, ret: ast.Node.Stmt.Return) Error!void {
    if (self.function.?.prototype.return_type == .void_type) {
        self.error_info = .{ .message = "return statements are not allowed when function's expected return type is void", .loc = ret.loc };

        return Error.UnexpectedReturn;
    }

    if (self.function.?.prototype.return_type != self.inferType(ret.value)) {
        var buf = std.ArrayList(u8).init(self.gpa);

        try buf.writer().print("expected return type '{s}' got '{s}'", .{ self.function.?.prototype.return_type.to_string(), self.inferType(ret.value).to_string() });

        self.error_info = .{ .message = try buf.toOwnedSlice(), .loc = ret.loc };

        return Error.MismatchedTypes;
    }

    try self.instructions.append(.{ .ret = .{ .value = try self.genValue(ret.value) } });
}

fn genValue(self: *CodeGen, expr: ast.Node.Expr) Error!IR.Value {
    return switch (expr) {
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
    _ = self;

    return switch (expr) {
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
