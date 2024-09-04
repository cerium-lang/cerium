const std = @import("std");

const Ast = @import("Ast.zig");
const Type = @import("type.zig").Type;
const SymbolTable = @import("SymbolTable.zig");

const Ir = @This();

instructions: []const Instruction,
string_literals: []const []const u8,

pub const Instruction = union(enum) {
    label: Label,
    function_proluge,
    function_epilogue,
    load: Load,
    store: Store,
    inline_assembly: InlineAssembly,
    @"return",

    pub const Label = struct {
        name: []const u8,
    };

    pub const Load = union(enum) {
        name: []const u8,
        string: usize,
        char: u8,
        int: i64,
        float: f64,
    };

    pub const Store = struct {
        name: []const u8,
    };

    pub const InlineAssembly = struct {
        content: []const u8,
    };
};

pub const Generator = struct {
    allocator: std.mem.Allocator,

    instructions: std.ArrayList(Ir.Instruction),
    string_literals: std.ArrayList([]const u8),

    function: ?Ast.Node.Stmt.FunctionDeclaration = null,
    function_returned: bool = false,

    symbol_table: SymbolTable,

    error_info: ?ErrorInfo = null,

    pub const Error = error{
        MismatchedTypes,
        ExpectedReturn,
        UnexpectedReturn,
        UndeclaredVariable,
        RedeclaredVariable,
        UnsupportedFeature,
    } || std.mem.Allocator.Error;

    pub const ErrorInfo = struct {
        message: []const u8,
        source_loc: Ast.SourceLoc,
    };

    pub fn init(allocator: std.mem.Allocator) Generator {
        return Generator{
            .allocator = allocator,
            .instructions = std.ArrayList(Ir.Instruction).init(allocator),
            .string_literals = std.ArrayList([]const u8).init(allocator),
            .symbol_table = SymbolTable.init(allocator),
        };
    }

    pub fn generate(self: *Generator, ast: Ast) Error!Ir {
        for (ast.body) |node| {
            try self.generateNode(node);
        }

        return Ir{
            .instructions = try self.instructions.toOwnedSlice(),
            .string_literals = try self.string_literals.toOwnedSlice(),
        };
    }

    fn generateNode(self: *Generator, node: Ast.Node) Error!void {
        switch (node) {
            .stmt => try self.generateStmt(node.stmt),
            .expr => {},
        }
    }

    fn generateStmt(self: *Generator, stmt: Ast.Node.Stmt) Error!void {
        switch (stmt) {
            .function_declaration => try self.generateFunctionDeclarationStmt(stmt.function_declaration),

            .variable_declaration => try self.generateVariableDeclarationStmt(stmt.variable_declaration),

            .inline_assembly => try self.generateInlineAssemblyStmt(stmt.inline_assembly),

            .@"return" => try self.generateReturnStmt(stmt.@"return"),
        }
    }

    fn generateFunctionDeclarationStmt(self: *Generator, function: Ast.Node.Stmt.FunctionDeclaration) Error!void {
        if (self.function != null) {
            self.error_info = .{ .message = "local functions are not supported yet", .source_loc = function.prototype.name.source_loc };

            return error.UnsupportedFeature;
        }

        try self.instructions.append(.{ .label = .{ .name = function.prototype.name.buffer } });

        try self.instructions.append(.function_proluge);

        self.function = function;

        for (self.function.?.body) |node| {
            try self.generateNode(node);
        }

        if (!self.function_returned) {
            if (self.function.?.prototype.return_type == .void_type) {
                try self.instructions.append(.function_epilogue);

                try self.instructions.append(.@"return");
            } else {
                self.error_info = .{ .message = "expected function with non-void return type to explicitly return", .source_loc = self.function.?.prototype.name.source_loc };

                return error.ExpectedReturn;
            }
        }

        self.symbol_table.reset();

        self.function = null;
        self.function_returned = false;
    }

    fn generateVariableDeclarationStmt(self: *Generator, variable: Ast.Node.Stmt.VariableDeclaration) Error!void {
        if (self.function == null) {
            self.error_info = .{ .message = "global variables are not supported yet", .source_loc = variable.name.source_loc };

            return error.UnsupportedFeature;
        }

        if (variable.type != self.inferType(variable.value)) {
            var buf = std.ArrayList(u8).init(self.allocator);

            try buf.writer().print("expected type '{s}' got '{s}'", .{ variable.type.to_string(), self.inferType(variable.value).to_string() });

            self.error_info = .{ .message = try buf.toOwnedSlice(), .source_loc = variable.name.source_loc };

            return error.MismatchedTypes;
        }

        try self.symbol_table.set(.{
            .name = variable.name,
            .type = variable.type,
        });

        try self.generateValue(variable.value);

        try self.instructions.append(.{ .store = .{ .name = variable.name.buffer } });
    }

    fn generateInlineAssemblyStmt(self: *Generator, inline_assembly: Ast.Node.Stmt.InlineAssembly) Error!void {
        try self.instructions.append(.{ .inline_assembly = .{ .content = inline_assembly.content } });
    }

    fn generateReturnStmt(self: *Generator, @"return": Ast.Node.Stmt.Return) Error!void {
        if (self.function.?.prototype.return_type == .void_type) {
            self.error_info = .{ .message = "didn't expect function with void return type to explicitly return", .source_loc = @"return".source_loc };

            return error.UnexpectedReturn;
        }

        if (self.function.?.prototype.return_type != self.inferType(@"return".value)) {
            var buf = std.ArrayList(u8).init(self.allocator);

            try buf.writer().print("expected return type '{s}' got '{s}'", .{ self.function.?.prototype.return_type.to_string(), self.inferType(@"return".value).to_string() });

            self.error_info = .{ .message = try buf.toOwnedSlice(), .source_loc = @"return".source_loc };

            return error.MismatchedTypes;
        }

        try self.generateValue(@"return".value);

        try self.instructions.append(.function_epilogue);

        try self.instructions.append(.@"return");

        self.function_returned = true;
    }

    fn generateValue(self: *Generator, expr: Ast.Node.Expr) Error!void {
        return switch (expr) {
            .identifier => {
                if (self.symbol_table.lookup(expr.identifier.name.buffer) == error.Undeclared) {
                    var buf = std.ArrayList(u8).init(self.allocator);

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

    fn inferType(self: Generator, expr: Ast.Node.Expr) Type {
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
};
