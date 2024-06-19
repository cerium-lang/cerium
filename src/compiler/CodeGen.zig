const std = @import("std");

const ast = @import("ast.zig");
const Type = @import("type.zig").Type;
const SymbolTable = @import("SymbolTable.zig");
const IR = @import("IR.zig");

const CodeGen = @This();

gpa: std.mem.Allocator,

instructions: std.ArrayList(IR.Instruction),
string_literals: std.ArrayList([]const u8),

function: ?ast.Declaration.Function = null,
function_returned: bool = false,

symbol_table: SymbolTable,

error_info: ?ErrorInfo = null,

pub const Error = error{
    MismatchedTypes,
    ExpectedReturn,
    UnexpectedReturn,
    UndeclaredVariable,
    RedeclaredVariable,
} || std.mem.Allocator.Error;

pub const ErrorInfo = struct {
    message: []const u8,
    source_loc: ast.SourceLoc,
};

pub fn init(gpa: std.mem.Allocator) CodeGen {
    return CodeGen{
        .gpa = gpa,
        .instructions = std.ArrayList(IR.Instruction).init(gpa),
        .string_literals = std.ArrayList([]const u8).init(gpa),
        .symbol_table = SymbolTable.init(gpa),
    };
}

pub fn compile(self: *CodeGen, root: ast.Root) Error!IR {
    for (root.declarations) |declaration| {
        try self.compileDeclaration(declaration);
    }

    return IR{
        .instructions = try self.instructions.toOwnedSlice(),
        .string_literals = try self.string_literals.toOwnedSlice(),
    };
}

fn compileDeclaration(self: *CodeGen, declaration: ast.Declaration) Error!void {
    switch (declaration) {
        .function => try self.compileFunctionDeclaration(declaration.function),
    }
}

fn compileFunctionDeclaration(self: *CodeGen, function: ast.Declaration.Function) Error!void {
    try self.instructions.append(.{ .label = .{ .name = function.prototype.name.buffer } });

    self.function = function;

    for (self.function.?.body) |node| {
        try self.compileNode(node);
    }

    if (!self.function_returned) {
        if (self.function.?.prototype.return_type == .void_type) {
            try self.instructions.append(.{ .@"return" = {} });
        } else {
            self.error_info = .{ .message = "expected function with non-void return type to explicitly return", .source_loc = self.function.?.prototype.name.source_loc };

            return error.ExpectedReturn;
        }
    }

    self.symbol_table.reset();

    self.function = null;
    self.function_returned = false;
}

fn compileNode(self: *CodeGen, node: ast.Node) Error!void {
    switch (node) {
        .stmt => try self.compileStmt(node.stmt),
        .expr => {},
    }
}

fn compileStmt(self: *CodeGen, stmt: ast.Node.Stmt) Error!void {
    switch (stmt) {
        .variable_declaration => try self.compileVariableDeclarationStmt(stmt.variable_declaration),

        .inline_assembly => try self.compileInlineAssemblyStmt(stmt.inline_assembly),

        .@"return" => try self.compileReturnStmt(stmt.@"return"),
    }
}

fn compileVariableDeclarationStmt(self: *CodeGen, variable: ast.Node.Stmt.VariableDeclaration) Error!void {
    if (variable.type != self.inferType(variable.value)) {
        var buf = std.ArrayList(u8).init(self.gpa);

        try buf.writer().print("expected type '{s}' got '{s}'", .{ variable.type.to_string(), self.inferType(variable.value).to_string() });

        self.error_info = .{ .message = try buf.toOwnedSlice(), .source_loc = variable.name.source_loc };

        return error.MismatchedTypes;
    }

    try self.symbol_table.set(.{
        .name = variable.name,
        .type = variable.type,
    });

    try self.compileValue(variable.value);

    try self.instructions.append(.{ .store = .{ .name = variable.name.buffer } });
}

fn compileInlineAssemblyStmt(self: *CodeGen, inline_assembly: ast.Node.Stmt.InlineAssembly) Error!void {
    try self.instructions.append(.{ .inline_assembly = .{ .content = inline_assembly.content } });
}

fn compileReturnStmt(self: *CodeGen, @"return": ast.Node.Stmt.Return) Error!void {
    if (self.function.?.prototype.return_type == .void_type) {
        self.error_info = .{ .message = "didn't expect function with void return type to explicitly return", .source_loc = @"return".source_loc };

        return error.UnexpectedReturn;
    }

    if (self.function.?.prototype.return_type != self.inferType(@"return".value)) {
        var buf = std.ArrayList(u8).init(self.gpa);

        try buf.writer().print("expected return type '{s}' got '{s}'", .{ self.function.?.prototype.return_type.to_string(), self.inferType(@"return".value).to_string() });

        self.error_info = .{ .message = try buf.toOwnedSlice(), .source_loc = @"return".source_loc };

        return error.MismatchedTypes;
    }

    try self.compileValue(@"return".value);

    try self.instructions.append(.{ .@"return" = {} });

    self.function_returned = true;
}

fn compileValue(self: *CodeGen, expr: ast.Node.Expr) Error!void {
    return switch (expr) {
        .identifier => {
            if (self.symbol_table.lookup(expr.identifier.name.buffer) == error.Undeclared) {
                var buf = std.ArrayList(u8).init(self.gpa);

                try buf.writer().print("{s} is not declared", .{expr.identifier.name.buffer});

                self.error_info = .{ .message = try buf.toOwnedSlice(), .source_loc = expr.identifier.name.source_loc };

                return error.UndeclaredVariable;
            }

            try self.instructions.append(.{ .load = .{ .name = expr.identifier.name.buffer } });
        },

        .string => {
            try self.string_literals.append(expr.string.value);

            const index = self.string_literals.items.len - 1;

            try self.instructions.append(.{ .load = .{ .string = index } });
        },

        .char => {
            try self.instructions.append(.{ .load = .{ .char = expr.char.value } });
        },

        .int => {
            try self.instructions.append(.{ .load = .{ .int = expr.int.value } });
        },

        .float => {
            try self.instructions.append(.{ .load = .{ .float = expr.float.value } });
        },
    };
}

fn inferType(self: CodeGen, expr: ast.Node.Expr) Type {
    return switch (expr) {
        .identifier => {
            const symbol = self.symbol_table.lookup(expr.identifier.name.buffer) catch {
                unreachable;
            };

            return symbol.type;
        },

        .string => .string_type,
        .char => .char_type,
        .int => .int_type,
        .float => .float_type,
    };
}
