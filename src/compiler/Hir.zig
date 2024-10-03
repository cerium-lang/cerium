//! High Intermediate Representation.
//!
//! An unchecked stack-based intermediate representation lowered from `Ast`.
//! Passed to `Sema` which checks all the instructions and types to be valid and lowers it down to `Lir`,
//! And then `Lir` gets lowered down to machine code.

const std = @import("std");

const Ast = @import("Ast.zig");
const Compilation = @import("Compilation.zig");
const Symbol = @import("Symbol.zig");
const SubType = Ast.SubType;

const Hir = @This();

external_variables: std.StringArrayHashMapUnmanaged(SubType) = .{},
global_blocks: std.StringArrayHashMapUnmanaged(Block) = .{},
functions: std.StringArrayHashMapUnmanaged(Function) = .{},

pub const Function = struct {
    prototype: Ast.Node.Stmt.FunctionDeclaration.Prototype,
    subtype: SubType,
    blocks: std.StringArrayHashMapUnmanaged(Block) = .{},
};

pub const SubSymbol = struct {
    name: Ast.Name,
    subtype: SubType,
    linkage: Symbol.Linkage,
};

pub const Block = struct {
    tag: Tag = .basic,
    instructions: std.ArrayListUnmanaged(Instruction) = .{},

    pub const Tag = enum {
        basic,
        control_flow,
    };

    pub const Instruction = union(enum) {
        /// Declare function parameters
        parameters,
        /// Call a specific function pointer on the stack with the specified argument count
        call: struct { usize, Ast.SourceLoc },
        /// Declare a constant that is only known at compile time and acts as a placeholder for a value
        constant: SubSymbol,
        /// Same as `constant` but the type is unknown at the point of declaration
        constant_infer: SubSymbol,
        /// Declare a variable that is only known at runtime and doesn't get replaced by the compiler
        variable: SubSymbol,
        /// Same as `variable` but the type is unknown at the point of declaration
        variable_infer: SubSymbol,
        /// Set a type alias using a name and a subtype
        type_alias: SubSymbol,
        /// Set a value using the specified name
        set: Ast.Name,
        /// Get a value using the specified name
        get: Ast.Name,
        /// Push a string onto the stack
        string: []const u8,
        /// Push an integer onto the stack
        int: i128,
        /// Push a float onto the stack
        float: f64,
        /// Negate an integer or float
        negate: Ast.SourceLoc,
        /// Reverse a boolean from true to false and from false to true
        bool_not: Ast.SourceLoc,
        /// Perform bitwise NOT operation on the bits of rhs (Which is to reverse its bits representation)
        bit_not: Ast.SourceLoc,
        /// Perform bitwise AND operation on the bits of lhs and rhs
        bit_and: Ast.SourceLoc,
        /// Perform bitwise OR operation on the bits of lhs and rhs
        bit_or: Ast.SourceLoc,
        /// Perform bitwise XOR operation on the bits of lhs and rhs
        bit_xor: Ast.SourceLoc,
        /// Get a pointer of a value on the stack
        reference: Ast.SourceLoc,
        /// Read the data that the pointer is pointing to
        read: Ast.SourceLoc,
        /// Override the data that the pointer is pointing to
        write: Ast.SourceLoc,
        /// Offset a pointer using byte alignment and type size as a factor
        offset: Ast.SourceLoc,
        /// Add two integers or floats or pointers on the top of the stack
        add: Ast.SourceLoc,
        /// Subtract two integers or floats or pointers on the top of the stack
        sub: Ast.SourceLoc,
        /// Multiply two integers or floats on the top of the stack
        mul: Ast.SourceLoc,
        /// Divide two integers or floats on the top of the stack
        div: Ast.SourceLoc,
        /// Compare between two integers or floats on the stack and check for order (in this case, lhs less than rhs)
        lt: Ast.SourceLoc,
        /// Compare between two integers or floats on the stack and check for order (in this case, lhs greater than rhs)
        gt: Ast.SourceLoc,
        /// Compare between two values on the stack and check for equality
        eql: Ast.SourceLoc,
        /// Shift to left the bits of lhs using rhs offset
        shl: Ast.SourceLoc,
        /// Shift to right the bits of lhs using rhs offset
        shr: Ast.SourceLoc,
        /// Jump to block if the value on stack is false
        jmp_if_false: Ast.Name,
        /// Jump to block
        jmp: []const u8,
        /// Place a machine-specific assembly in the output
        assembly: Ast.Node.Expr.Assembly,
        /// Pop a value from the stack
        pop,
        /// Return out of the function
        @"return",
    };
};

pub fn deinit(self: *Hir, allocator: std.mem.Allocator) void {
    for (self.global_blocks.values()) |*block| {
        block.instructions.deinit(allocator);
    }

    self.global_blocks.deinit(allocator);

    self.external_variables.deinit(allocator);

    for (self.functions.values()) |*function| {
        for (function.blocks.values()) |*block| {
            block.instructions.deinit(allocator);
        }

        function.blocks.deinit(allocator);
    }

    self.functions.deinit(allocator);
}

pub const Generator = struct {
    allocator: std.mem.Allocator,

    env: Compilation.Environment,

    hir: Hir = .{},

    maybe_hir_function: ?*Hir.Function = null,
    maybe_hir_block: ?*Hir.Block = null,

    error_info: ?ErrorInfo = null,

    pub const ErrorInfo = struct {
        message: []const u8,
        source_loc: Ast.SourceLoc,
    };

    pub const Error = error{
        UnexpectedStatement,
        UnexpectedExpression,
        UnsupportedFeature,
        Redeclared,
    } || std.mem.Allocator.Error;

    pub fn init(allocator: std.mem.Allocator, env: Compilation.Environment) Generator {
        return Generator{
            .allocator = allocator,
            .env = env,
        };
    }

    pub fn generate(self: *Generator, ast: Ast) Error!void {
        for (ast.body) |node| {
            try self.generateNode(node);
        }
    }

    fn generateNode(self: *Generator, node: Ast.Node) Error!void {
        switch (node) {
            .stmt => |stmt| try self.generateStmt(stmt),
            .expr => |expr| {
                if (self.maybe_hir_block) |hir_block| {
                    try self.generateExpr(expr);

                    try hir_block.instructions.append(self.allocator, .pop);
                } else {
                    self.error_info = .{ .message = "did not expect an expression to be in top level", .source_loc = expr.getSourceLoc() };

                    return error.UnexpectedExpression;
                }
            },
        }
    }

    fn generateStmt(self: *Generator, stmt: Ast.Node.Stmt) Error!void {
        switch (stmt) {
            .function_declaration => |function_declaration| try self.generateFunctionDeclarationStmt(function_declaration),
            .variable_declaration => |variable_declaration| try self.generateVariableDeclarationStmt(variable_declaration),

            .type_alias => |type_alias| try self.generateTypeAliasStmt(type_alias),

            .conditional => |conditional| try self.generateConditionalStmt(conditional),

            .while_loop => |while_loop| try self.generateWhileLoopStmt(while_loop),
            .@"continue" => |@"continue"| try self.generateContinueStmt(@"continue"),
            .@"break" => |@"break"| try self.generateBreakStmt(@"break"),

            .@"return" => |@"return"| try self.generateReturnStmt(@"return"),
        }
    }

    fn reportRedeclaration(self: *Generator, name: Ast.Name) Error!void {
        var error_message_buf: std.ArrayListUnmanaged(u8) = .{};

        try error_message_buf.writer(self.allocator).print("redeclaration of '{s}'", .{name.buffer});

        self.error_info = .{ .message = error_message_buf.items, .source_loc = name.source_loc };

        return error.Redeclared;
    }

    fn generateFunctionDeclarationStmt(self: *Generator, ast_function: Ast.Node.Stmt.FunctionDeclaration) Error!void {
        if (self.hir.external_variables.get(ast_function.prototype.name.buffer) != null or
            self.hir.global_blocks.get(ast_function.prototype.name.buffer) != null or
            self.hir.functions.get(ast_function.prototype.name.buffer) != null)
        {
            return self.reportRedeclaration(ast_function.prototype.name);
        }

        var parameter_subtypes = try self.allocator.alloc(SubType, ast_function.prototype.parameters.len);

        for (ast_function.prototype.parameters, 0..) |ast_function_parameter, i| {
            parameter_subtypes[i] = ast_function_parameter.expected_subtype;
        }

        const return_subtype_on_heap = try self.allocator.create(SubType);
        return_subtype_on_heap.* = ast_function.prototype.return_subtype;

        const function_subtype: SubType = .{
            .function = .{
                .parameter_subtypes = parameter_subtypes,
                .return_subtype = return_subtype_on_heap,
            },
        };

        if (ast_function.prototype.is_external) {
            return self.hir.external_variables.put(self.allocator, ast_function.prototype.name.buffer, function_subtype);
        }

        if (self.maybe_hir_block != null) {
            self.error_info = .{ .message = "cannot declare functions inside other functions", .source_loc = ast_function.prototype.name.source_loc };

            return error.UnexpectedStatement;
        }

        const new_hir_function_entry = try self.hir.functions.getOrPutValue(
            self.allocator,
            ast_function.prototype.name.buffer,
            .{
                .prototype = ast_function.prototype,
                .subtype = function_subtype,
            },
        );

        const new_hir_function = new_hir_function_entry.value_ptr;

        const new_hir_block_entry = try new_hir_function.blocks.getOrPutValue(self.allocator, "entry", .{});

        const new_hir_block = new_hir_block_entry.value_ptr;

        try new_hir_block.instructions.append(self.allocator, .parameters);

        self.maybe_hir_function = new_hir_function;
        defer self.maybe_hir_function = null;

        self.maybe_hir_block = new_hir_block;
        defer self.maybe_hir_block = null;

        for (ast_function.body) |node| {
            try self.generateNode(node);
        }

        try self.maybe_hir_block.?.instructions.append(self.allocator, .@"return");
    }

    fn emitVariable(allocator: std.mem.Allocator, hir_block: *Hir.Block, variable: Ast.Node.Stmt.VariableDeclaration, linkage: Symbol.Linkage) Error!void {
        try hir_block.instructions.append(
            allocator,
            if (variable.subtype) |@"type"|
                if (variable.is_const)
                    .{
                        .constant = .{
                            .name = variable.name,
                            .subtype = @"type",
                            .linkage = linkage,
                        },
                    }
                else
                    .{
                        .variable = .{
                            .name = variable.name,
                            .subtype = @"type",
                            .linkage = linkage,
                        },
                    }
            else if (variable.is_const)
                .{
                    .constant_infer = .{
                        .name = variable.name,
                        .subtype = .{ .pure = .void },
                        .linkage = linkage,
                    },
                }
            else
                .{
                    .variable_infer = .{
                        .name = variable.name,
                        .subtype = .{ .pure = .void },
                        .linkage = linkage,
                    },
                },
        );
    }

    fn generateVariableDeclarationStmt(self: *Generator, variable: Ast.Node.Stmt.VariableDeclaration) Error!void {
        if (self.hir.external_variables.get(variable.name.buffer) != null or
            self.hir.global_blocks.get(variable.name.buffer) != null or
            self.hir.functions.get(variable.name.buffer) != null)
        {
            return self.reportRedeclaration(variable.name);
        }

        if (variable.is_external) {
            try self.hir.external_variables.put(self.allocator, variable.name.buffer, variable.subtype.?);
        } else if (self.maybe_hir_block) |hir_block| {
            try self.generateExpr(variable.value);

            try emitVariable(self.allocator, hir_block, variable, .local);

            try hir_block.instructions.append(self.allocator, .{ .set = variable.name });
        } else {
            const new_hir_block_entry = try self.hir.global_blocks.getOrPutValue(self.allocator, variable.name.buffer, .{});

            const new_hir_block = new_hir_block_entry.value_ptr;

            self.maybe_hir_block = new_hir_block;
            defer self.maybe_hir_block = null;

            try self.generateExpr(variable.value);

            try emitVariable(self.allocator, new_hir_block, variable, .global);

            try new_hir_block.instructions.append(self.allocator, .{ .set = variable.name });
        }
    }

    fn generateTypeAliasStmt(self: *Generator, type_alias: Ast.Node.Stmt.TypeAlias) Error!void {
        if (self.hir.external_variables.get(type_alias.name.buffer) != null or
            self.hir.global_blocks.get(type_alias.name.buffer) != null or
            self.hir.functions.get(type_alias.name.buffer) != null)
        {
            return self.reportRedeclaration(type_alias.name);
        }

        if (self.maybe_hir_block) |hir_block| {
            try hir_block.instructions.append(
                self.allocator,
                .{
                    .type_alias = .{
                        .name = type_alias.name,
                        .subtype = type_alias.subtype,
                        .linkage = .local,
                    },
                },
            );
        } else {
            const new_hir_block_entry = try self.hir.global_blocks.getOrPutValue(self.allocator, type_alias.name.buffer, .{});

            const new_hir_block = new_hir_block_entry.value_ptr;

            self.maybe_hir_block = new_hir_block;
            defer self.maybe_hir_block = null;

            try new_hir_block.instructions.append(
                self.allocator,
                .{
                    .type_alias = .{
                        .name = type_alias.name,
                        .subtype = type_alias.subtype,
                        .linkage = .global,
                    },
                },
            );
        }
    }

    var conditional_parts_emitted: usize = 0;

    fn generateConditionalStmt(self: *Generator, conditional: Ast.Node.Stmt.Conditional) Error!void {
        if (self.maybe_hir_function) |hir_function| {
            std.debug.assert(conditional.conditions.len >= 1);
            std.debug.assert(conditional.possiblities.len >= 1);

            var fallback_block_name: std.ArrayListUnmanaged(u8) = .{};
            try fallback_block_name.writer(self.allocator).print("conditional.fallback{}", .{conditional_parts_emitted});

            var end_block_name: std.ArrayListUnmanaged(u8) = .{};
            try end_block_name.writer(self.allocator).print("conditional.end{}", .{conditional_parts_emitted});

            var next_possiblity_block_name: std.ArrayListUnmanaged(u8) = .{};
            try next_possiblity_block_name.writer(self.allocator).print("conditional.possiblity{}", .{conditional_parts_emitted});

            for (conditional.conditions, conditional.possiblities, 0..) |condition, possiblity, i| {
                const possiblity_block_name = next_possiblity_block_name;

                conditional_parts_emitted += 1;

                next_possiblity_block_name = .{};
                try next_possiblity_block_name.writer(self.allocator).print("conditional.possiblity{}", .{conditional_parts_emitted});

                const possiblity_block_entry = try hir_function.blocks.getOrPutValue(self.allocator, possiblity_block_name.items, .{
                    .tag = .control_flow,
                });

                const possiblity_block = possiblity_block_entry.value_ptr;

                self.maybe_hir_block = possiblity_block;

                try self.generateExpr(condition);

                try possiblity_block.instructions.append(
                    self.allocator,
                    .{
                        .jmp_if_false = .{
                            .buffer = if (i == conditional.possiblities.len - 1) fallback_block_name.items else next_possiblity_block_name.items,
                            .source_loc = condition.getSourceLoc(),
                        },
                    },
                );

                for (possiblity) |possibility_node| {
                    try self.generateNode(possibility_node);
                }

                try possiblity_block.instructions.append(self.allocator, .{ .jmp = end_block_name.items });
            }

            {
                const fallback_block_entry = try hir_function.blocks.getOrPutValue(self.allocator, fallback_block_name.items, .{
                    .tag = .control_flow,
                });

                self.maybe_hir_block = fallback_block_entry.value_ptr;

                for (conditional.fallback) |fallback_node| {
                    try self.generateNode(fallback_node);
                }
            }

            {
                const end_block_entry = try hir_function.blocks.getOrPutValue(self.allocator, end_block_name.items, .{});

                self.maybe_hir_block = end_block_entry.value_ptr;
            }
        } else {
            self.error_info = .{ .message = "expected the conditional statement to be inside a function", .source_loc = conditional.conditions[0].getSourceLoc() };

            return error.UnexpectedStatement;
        }
    }

    var loop_parts_emitted: usize = 0;

    var loop_begin_block_name: ?std.ArrayListUnmanaged(u8) = null;
    var loop_end_block_name: ?std.ArrayListUnmanaged(u8) = .{};

    fn generateWhileLoopStmt(self: *Generator, while_loop: Ast.Node.Stmt.WhileLoop) Error!void {
        if (self.maybe_hir_function) |hir_function| {
            loop_begin_block_name = .{};
            loop_end_block_name = .{};
            defer loop_begin_block_name = null;
            defer loop_end_block_name = null;

            const begin_block_name = &loop_begin_block_name.?;
            const end_block_name = &loop_end_block_name.?;

            try begin_block_name.writer(self.allocator).print("loop.begin{}", .{loop_parts_emitted});
            try end_block_name.writer(self.allocator).print("loop.end{}", .{loop_parts_emitted});

            {
                const begin_block_entry = try hir_function.blocks.getOrPutValue(self.allocator, begin_block_name.items, .{
                    .tag = .control_flow,
                });

                const begin_block = begin_block_entry.value_ptr;

                self.maybe_hir_block = begin_block;

                try self.generateExpr(while_loop.condition);

                try begin_block.instructions.append(
                    self.allocator,
                    .{
                        .jmp_if_false = .{
                            .buffer = end_block_name.items,
                            .source_loc = while_loop.condition.getSourceLoc(),
                        },
                    },
                );

                for (while_loop.body) |node| {
                    try self.generateNode(node);
                }

                try begin_block.instructions.append(self.allocator, .{ .jmp = begin_block_name.items });

                loop_parts_emitted += 1;
            }

            {
                const end_block_entry = try hir_function.blocks.getOrPutValue(self.allocator, end_block_name.items, .{});

                self.maybe_hir_block = end_block_entry.value_ptr;
            }
        } else {
            self.error_info = .{ .message = "expected the while loop statement to be inside a function", .source_loc = while_loop.condition.getSourceLoc() };

            return error.UnexpectedStatement;
        }
    }

    fn generateContinueStmt(self: *Generator, @"continue": Ast.Node.Stmt.Continue) Error!void {
        if (self.maybe_hir_block) |hir_block| {
            if (loop_begin_block_name) |begin_block_name| {
                return hir_block.instructions.append(self.allocator, .{ .jmp = begin_block_name.items });
            }
        }

        self.error_info = .{ .message = "expected the continue statement to be inside a loop", .source_loc = @"continue".source_loc };

        return error.UnexpectedStatement;
    }

    fn generateBreakStmt(self: *Generator, @"break": Ast.Node.Stmt.Break) Error!void {
        if (self.maybe_hir_block) |hir_block| {
            if (loop_end_block_name) |end_block_name| {
                return hir_block.instructions.append(self.allocator, .{ .jmp = end_block_name.items });
            }
        }

        self.error_info = .{ .message = "expected the break statement to be inside a loop", .source_loc = @"break".source_loc };

        return error.UnexpectedStatement;
    }

    fn generateReturnStmt(self: *Generator, @"return": Ast.Node.Stmt.Return) Error!void {
        if (self.maybe_hir_block) |hir_block| {
            try self.generateExpr(@"return".value);

            try hir_block.instructions.append(self.allocator, .@"return");
        } else {
            self.error_info = .{ .message = "expected the return statement to be inside a function", .source_loc = @"return".source_loc };

            return error.UnexpectedStatement;
        }
    }

    fn generateExpr(self: *Generator, expr: Ast.Node.Expr) Error!void {
        switch (expr) {
            .identifier => |identifier| try self.generateIdentifierExpr(identifier),

            .string => |string| try self.generateStringExpr(string),

            .int => |int| try self.generateIntExpr(int),

            .float => |float| try self.generateFloatExpr(float),

            .assembly => |assembly| try self.generateAssemblyExpr(assembly),

            .unary_operation => |unary_operation| try self.generateUnaryOperationExpr(unary_operation),

            .binary_operation => |binary_operation| try self.generateBinaryOperationExpr(binary_operation),

            .subscript => |subscript| try self.generateSubscript(subscript),

            .call => |call| try self.generateCallExpr(call),
        }
    }

    fn generateIdentifierExpr(self: *Generator, identifier: Ast.Node.Expr.Identifier) Error!void {
        try self.maybe_hir_block.?.instructions.append(self.allocator, .{ .get = identifier.name });
    }

    fn generateStringExpr(self: *Generator, string: Ast.Node.Expr.String) Error!void {
        try self.maybe_hir_block.?.instructions.append(self.allocator, .{ .string = string.value });
    }

    fn generateIntExpr(self: *Generator, int: Ast.Node.Expr.Int) Error!void {
        try self.maybe_hir_block.?.instructions.append(self.allocator, .{ .int = int.value });
    }

    fn generateFloatExpr(self: *Generator, float: Ast.Node.Expr.Float) Error!void {
        try self.maybe_hir_block.?.instructions.append(self.allocator, .{ .float = float.value });
    }

    fn generateAssemblyExpr(self: *Generator, assembly: Ast.Node.Expr.Assembly) Error!void {
        var i: usize = assembly.input_constraints.len;

        while (i > 0) {
            i -= 1;

            const input_constraint = assembly.input_constraints[i];

            try self.generateExpr(input_constraint.value.*);
        }

        try self.maybe_hir_block.?.instructions.append(self.allocator, .{ .assembly = assembly });
    }

    fn generateUnaryOperationExpr(self: *Generator, unary_operation: Ast.Node.Expr.UnaryOperation) Error!void {
        switch (unary_operation.operator) {
            .minus => {
                try self.generateExpr(unary_operation.rhs.*);
                try self.maybe_hir_block.?.instructions.append(self.allocator, .{ .negate = unary_operation.source_loc });
            },

            .bang => {
                try self.generateExpr(unary_operation.rhs.*);
                try self.maybe_hir_block.?.instructions.append(self.allocator, .{ .bool_not = unary_operation.source_loc });
            },

            .tilde => {
                try self.generateExpr(unary_operation.rhs.*);
                try self.maybe_hir_block.?.instructions.append(self.allocator, .{ .bit_not = unary_operation.source_loc });
            },

            .ampersand => {
                if (unary_operation.rhs.* == .subscript) {
                    try self.generateExpr(unary_operation.rhs.subscript.target.*);
                    try self.generateExpr(unary_operation.rhs.subscript.index.*);

                    try self.maybe_hir_block.?.instructions.append(self.allocator, .{ .offset = unary_operation.rhs.subscript.source_loc });
                } else {
                    try self.generateExpr(unary_operation.rhs.*);

                    try self.maybe_hir_block.?.instructions.append(self.allocator, .{ .reference = unary_operation.source_loc });
                }
            },

            .star => {
                try self.generateExpr(unary_operation.rhs.*);
                try self.maybe_hir_block.?.instructions.append(self.allocator, .{ .read = unary_operation.source_loc });
            },
        }
    }

    fn generateBinaryOperationExpr(self: *Generator, binary_operation: Ast.Node.Expr.BinaryOperation) Error!void {
        switch (binary_operation.operator) {
            .plus => {
                try self.generateExpr(binary_operation.lhs.*);
                try self.generateExpr(binary_operation.rhs.*);

                try self.maybe_hir_block.?.instructions.append(self.allocator, .{ .add = binary_operation.source_loc });
            },

            .minus => {
                try self.generateExpr(binary_operation.lhs.*);
                try self.generateExpr(binary_operation.rhs.*);

                try self.maybe_hir_block.?.instructions.append(self.allocator, .{ .sub = binary_operation.source_loc });
            },

            .star => {
                try self.generateExpr(binary_operation.lhs.*);
                try self.generateExpr(binary_operation.rhs.*);

                try self.maybe_hir_block.?.instructions.append(self.allocator, .{ .mul = binary_operation.source_loc });
            },

            .forward_slash => {
                try self.generateExpr(binary_operation.lhs.*);
                try self.generateExpr(binary_operation.rhs.*);

                try self.maybe_hir_block.?.instructions.append(self.allocator, .{ .div = binary_operation.source_loc });
            },

            .equal_sign => {
                if (binary_operation.lhs.* == .unary_operation and binary_operation.lhs.unary_operation.operator == .star) {
                    try self.generateExpr(binary_operation.lhs.unary_operation.rhs.*);
                    try self.generateExpr(binary_operation.rhs.*);

                    try self.maybe_hir_block.?.instructions.append(self.allocator, .{ .write = binary_operation.source_loc });
                } else if (binary_operation.lhs.* == .identifier) {
                    try self.generateExpr(binary_operation.rhs.*);

                    try self.maybe_hir_block.?.instructions.append(self.allocator, .{ .set = binary_operation.lhs.identifier.name });
                } else {
                    self.error_info = .{ .message = "expected an identifier or a pointer dereference", .source_loc = binary_operation.lhs.getSourceLoc() };

                    return error.UnexpectedExpression;
                }

                try self.generateExpr(binary_operation.rhs.*);
            },

            .less_than => {
                try self.generateExpr(binary_operation.lhs.*);
                try self.generateExpr(binary_operation.rhs.*);

                try self.maybe_hir_block.?.instructions.append(self.allocator, .{ .lt = binary_operation.source_loc });
            },

            .greater_than => {
                try self.generateExpr(binary_operation.lhs.*);
                try self.generateExpr(binary_operation.rhs.*);

                try self.maybe_hir_block.?.instructions.append(self.allocator, .{ .gt = binary_operation.source_loc });
            },

            .double_less_than => {
                try self.generateExpr(binary_operation.lhs.*);
                try self.generateExpr(binary_operation.rhs.*);

                try self.maybe_hir_block.?.instructions.append(self.allocator, .{ .shl = binary_operation.source_loc });
            },

            .double_greater_than => {
                try self.generateExpr(binary_operation.lhs.*);
                try self.generateExpr(binary_operation.rhs.*);

                try self.maybe_hir_block.?.instructions.append(self.allocator, .{ .shr = binary_operation.source_loc });
            },

            .ampersand => {
                try self.generateExpr(binary_operation.lhs.*);
                try self.generateExpr(binary_operation.rhs.*);

                try self.maybe_hir_block.?.instructions.append(self.allocator, .{ .bit_and = binary_operation.source_loc });
            },

            .pipe => {
                try self.generateExpr(binary_operation.lhs.*);
                try self.generateExpr(binary_operation.rhs.*);

                try self.maybe_hir_block.?.instructions.append(self.allocator, .{ .bit_or = binary_operation.source_loc });
            },

            .caret => {
                try self.generateExpr(binary_operation.lhs.*);
                try self.generateExpr(binary_operation.rhs.*);

                try self.maybe_hir_block.?.instructions.append(self.allocator, .{ .bit_xor = binary_operation.source_loc });
            },

            .double_equal_sign, .bang_equal_sign => {
                try self.generateExpr(binary_operation.lhs.*);
                try self.generateExpr(binary_operation.rhs.*);

                try self.maybe_hir_block.?.instructions.append(self.allocator, .{ .eql = binary_operation.source_loc });

                if (binary_operation.operator == .bang_equal_sign) {
                    try self.maybe_hir_block.?.instructions.append(self.allocator, .{ .bool_not = binary_operation.source_loc });
                }
            },
        }
    }

    fn generateSubscript(self: *Generator, subscript: Ast.Node.Expr.Subscript) Error!void {
        try self.generateExpr(subscript.target.*);
        try self.generateExpr(subscript.index.*);

        try self.maybe_hir_block.?.instructions.append(self.allocator, .{ .offset = subscript.source_loc });
        try self.maybe_hir_block.?.instructions.append(self.allocator, .{ .read = subscript.source_loc });
    }

    fn generateCallExpr(self: *Generator, call: Ast.Node.Expr.Call) Error!void {
        var i: usize = call.arguments.len;

        while (i > 0) {
            i -= 1;

            const argument = call.arguments[i];

            try self.generateExpr(argument);
        }

        try self.generateExpr(call.callable.*);

        try self.maybe_hir_block.?.instructions.append(self.allocator, .{ .call = .{ call.arguments.len, call.source_loc } });
    }
};
