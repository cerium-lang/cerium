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

instructions: std.ArrayListUnmanaged(Instruction) = .{},

pub const SubSymbol = struct {
    name: Ast.Name,
    subtype: SubType,
    linkage: Symbol.Linkage,
};

pub const Instruction = union(enum) {
    /// Duplicate the top of the stack
    duplicate,
    /// Pop the top of the stack
    pop,
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
    /// Place a machine-specific assembly in the output
    assembly: Ast.Node.Expr.Assembly,
    /// Call a function pointer on the stack
    call: Call,
    /// Declare a function
    function: Function,
    /// Declare function parameters
    parameters,
    /// Declare a constant that is replaced at compile time and acts as a placeholder for a value
    constant: SubSymbol,
    /// Same as `constant` but the type is unknown at the point of declaration
    constant_infer: SubSymbol,
    /// Declare a variable that is only known at runtime and doesn't get replaced by the compiler
    variable: SubSymbol,
    /// Same as `variable` but the type is unknown at the point of declaration
    variable_infer: SubSymbol,
    /// Same as `variable` but the variable is external
    external: SubSymbol,
    /// Set a type alias
    type_alias: SubSymbol,
    /// Set a valua of a variable with a value on top of the stack
    set: Ast.Name,
    /// Get a value of a variable
    get: Ast.Name,
    /// Make a new block out of instructions
    block: Block,
    /// Unconditionally branch to a block
    br: Br,
    /// Conditionally branch to a block, condition is on the stack
    cond_br: CondBr,
    /// Start a new scope
    start_scope,
    /// End a scope
    end_scope,
    /// Return out of the function with a value on the stack
    ret,
    /// Return out of the function without a value
    ret_void,

    pub const Function = struct {
        prototype: Ast.Node.Stmt.FunctionDeclaration.Prototype,
        subtype: SubType,
    };

    pub const Call = struct {
        arguments_count: usize,
        source_loc: Ast.SourceLoc,
    };

    pub const Block = struct {
        id: u32,
    };

    pub const Br = struct {
        id: u32,
    };

    pub const CondBr = struct {
        true_id: u32,
        false_id: u32,
        source_loc: Ast.SourceLoc,
    };
};

pub const Generator = struct {
    allocator: std.mem.Allocator,

    env: Compilation.Environment,

    hir: Hir = .{},

    block_id: ?u32 = null,

    error_info: ?ErrorInfo = null,

    pub const ErrorInfo = struct {
        message: []const u8,
        source_loc: Ast.SourceLoc,
    };

    pub const Error = error{
        UnexpectedStatement,
        UnexpectedExpression,
        UnsupportedFeature,
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
                if (self.block_id == null) {
                    self.error_info = .{ .message = "did not expect an expression to be in top level", .source_loc = expr.getSourceLoc() };

                    return error.UnexpectedExpression;
                }

                try self.generateExpr(expr);

                try self.hir.instructions.append(self.allocator, .pop);
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

    fn generateFunctionDeclarationStmt(self: *Generator, ast_function: Ast.Node.Stmt.FunctionDeclaration) Error!void {
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

        const function_subtype_on_heap = try self.allocator.create(SubType);
        function_subtype_on_heap.* = function_subtype;

        const function_pointer_subtype: SubType = .{
            .pointer = .{
                .size = .one,
                .is_const = true,
                .is_local = false,
                .child_subtype = function_subtype_on_heap,
            },
        };

        if (ast_function.prototype.is_external) {
            return self.hir.instructions.append(
                self.allocator,
                .{
                    .external = .{
                        .name = ast_function.prototype.name,
                        .subtype = function_pointer_subtype,
                        .linkage = .global,
                    },
                },
            );
        }

        try self.hir.instructions.append(self.allocator, .{
            .function = .{
                .prototype = ast_function.prototype,
                .subtype = function_pointer_subtype,
            },
        });

        try self.hir.instructions.append(self.allocator, .{ .block = .{ .id = 0 } });

        self.block_id = 1;
        defer self.block_id = null;

        try self.hir.instructions.append(self.allocator, .start_scope);

        if (ast_function.prototype.parameters.len != 0) {
            try self.hir.instructions.append(self.allocator, .parameters);
        }

        for (ast_function.body) |node| {
            try self.generateNode(node);
        }

        if (self.hir.instructions.items.len == 0 or
            (self.hir.instructions.getLast() != .ret and
            self.hir.instructions.getLast() != .ret_void))
        {
            try self.hir.instructions.append(self.allocator, .ret_void);
        }

        try self.hir.instructions.append(self.allocator, .end_scope);
    }

    fn emitVariable(self: *Generator, variable: Ast.Node.Stmt.VariableDeclaration, linkage: Symbol.Linkage) Error!void {
        try self.hir.instructions.append(
            self.allocator,
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
        if (variable.is_external) {
            return self.hir.instructions.append(
                self.allocator,
                .{
                    .external = .{
                        .name = variable.name,
                        .subtype = variable.subtype.?,
                        .linkage = .global,
                    },
                },
            );
        }

        try self.generateExpr(variable.value);

        try self.emitVariable(variable, if (self.block_id == null) .global else .local);

        try self.hir.instructions.append(self.allocator, .{ .set = variable.name });
    }

    fn generateTypeAliasStmt(self: *Generator, type_alias: Ast.Node.Stmt.TypeAlias) Error!void {
        try self.hir.instructions.append(
            self.allocator,
            .{
                .type_alias = .{
                    .name = type_alias.name,
                    .subtype = type_alias.subtype,
                    .linkage = if (self.block_id == null) .global else .local,
                },
            },
        );
    }

    fn generateConditionalStmt(self: *Generator, conditional: Ast.Node.Stmt.Conditional) Error!void {
        std.debug.assert(conditional.conditions.len >= 1);
        std.debug.assert(conditional.possiblities.len >= 1);

        if (self.block_id == null) {
            self.error_info = .{ .message = "expected the conditional statement to be inside a function", .source_loc = conditional.conditions[0].getSourceLoc() };

            return error.UnexpectedStatement;
        }

        const end_block_id = self.block_id.?;
        self.block_id.? += 1;

        for (conditional.conditions, conditional.possiblities) |condition, possiblity| {
            try self.generateExpr(condition);

            try self.hir.instructions.append(self.allocator, .{
                .cond_br = .{
                    .true_id = self.block_id.?,
                    .false_id = self.block_id.? + 1,
                    .source_loc = condition.getSourceLoc(),
                },
            });

            try self.hir.instructions.append(self.allocator, .{ .block = .{ .id = self.block_id.? } });
            self.block_id.? += 1;

            try self.hir.instructions.append(self.allocator, .start_scope);

            for (possiblity) |possibility_node| {
                try self.generateNode(possibility_node);
            }

            try self.hir.instructions.append(self.allocator, .end_scope);

            try self.hir.instructions.append(self.allocator, .{ .br = .{ .id = end_block_id } });
        }

        {
            try self.hir.instructions.append(self.allocator, .{ .block = .{ .id = self.block_id.? } });
            self.block_id.? += 1;

            try self.hir.instructions.append(self.allocator, .start_scope);

            for (conditional.fallback) |fallback_node| {
                try self.generateNode(fallback_node);
            }

            try self.hir.instructions.append(self.allocator, .end_scope);

            try self.hir.instructions.append(self.allocator, .{ .br = .{ .id = end_block_id } });
        }

        try self.hir.instructions.append(self.allocator, .{ .block = .{ .id = end_block_id } });
    }

    var maybe_header_block_id: ?u32 = null;
    var maybe_end_block_id: ?u32 = null;

    fn generateWhileLoopStmt(self: *Generator, while_loop: Ast.Node.Stmt.WhileLoop) Error!void {
        if (self.block_id == null) {
            self.error_info = .{ .message = "expected the while loop statement to be inside a function", .source_loc = while_loop.condition.getSourceLoc() };

            return error.UnexpectedStatement;
        }

        const header_block_id = self.block_id.?;
        self.block_id.? += 1;

        const previous_header_block_id = maybe_header_block_id;
        maybe_header_block_id = header_block_id;
        defer maybe_header_block_id = previous_header_block_id;

        const body_block_id = self.block_id.?;
        self.block_id.? += 1;

        const end_block_id = self.block_id.?;
        self.block_id.? += 1;

        const previous_end_block_id = maybe_end_block_id;
        maybe_end_block_id = end_block_id;
        defer maybe_end_block_id = previous_end_block_id;

        try self.hir.instructions.append(self.allocator, .{ .br = .{ .id = header_block_id } });

        try self.hir.instructions.append(self.allocator, .{ .block = .{ .id = header_block_id } });

        try self.generateExpr(while_loop.condition);

        try self.hir.instructions.append(self.allocator, .{ .cond_br = .{
            .true_id = body_block_id,
            .false_id = end_block_id,
            .source_loc = while_loop.condition.getSourceLoc(),
        } });

        try self.hir.instructions.append(self.allocator, .{ .block = .{ .id = body_block_id } });

        try self.hir.instructions.append(self.allocator, .start_scope);

        for (while_loop.body) |node| {
            try self.generateNode(node);
        }

        try self.hir.instructions.append(self.allocator, .end_scope);

        try self.hir.instructions.append(self.allocator, .{ .br = .{ .id = header_block_id } });

        try self.hir.instructions.append(self.allocator, .{ .block = .{ .id = end_block_id } });
    }

    fn generateContinueStmt(self: *Generator, @"continue": Ast.Node.Stmt.Continue) Error!void {
        if (maybe_header_block_id) |header_block_id| {
            return self.hir.instructions.append(self.allocator, .{ .br = .{ .id = header_block_id } });
        }

        self.error_info = .{ .message = "expected the continue statement to be inside a loop", .source_loc = @"continue".source_loc };

        return error.UnexpectedStatement;
    }

    fn generateBreakStmt(self: *Generator, @"break": Ast.Node.Stmt.Break) Error!void {
        if (maybe_end_block_id) |end_block_id| {
            return self.hir.instructions.append(self.allocator, .{ .br = .{ .id = end_block_id } });
        }

        self.error_info = .{ .message = "expected the break statement to be inside a loop", .source_loc = @"break".source_loc };

        return error.UnexpectedStatement;
    }

    fn generateReturnStmt(self: *Generator, @"return": Ast.Node.Stmt.Return) Error!void {
        if (self.block_id == null) {
            self.error_info = .{ .message = "expected the return statement to be inside a function", .source_loc = @"return".source_loc };

            return error.UnexpectedStatement;
        }

        if (@"return".maybe_value) |value| {
            try self.generateExpr(value);

            try self.hir.instructions.append(self.allocator, .ret);
        } else {
            try self.hir.instructions.append(self.allocator, .ret_void);
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
        try self.hir.instructions.append(self.allocator, .{ .get = identifier.name });
    }

    fn generateStringExpr(self: *Generator, string: Ast.Node.Expr.String) Error!void {
        try self.hir.instructions.append(self.allocator, .{ .string = string.value });
    }

    fn generateIntExpr(self: *Generator, int: Ast.Node.Expr.Int) Error!void {
        try self.hir.instructions.append(self.allocator, .{ .int = int.value });
    }

    fn generateFloatExpr(self: *Generator, float: Ast.Node.Expr.Float) Error!void {
        try self.hir.instructions.append(self.allocator, .{ .float = float.value });
    }

    fn generateAssemblyExpr(self: *Generator, assembly: Ast.Node.Expr.Assembly) Error!void {
        var i: usize = assembly.input_constraints.len;

        while (i > 0) {
            i -= 1;

            const input_constraint = assembly.input_constraints[i];

            try self.generateExpr(input_constraint.value.*);
        }

        try self.hir.instructions.append(self.allocator, .{ .assembly = assembly });
    }

    fn generateUnaryOperationExpr(self: *Generator, unary_operation: Ast.Node.Expr.UnaryOperation) Error!void {
        switch (unary_operation.operator) {
            .minus => {
                try self.generateExpr(unary_operation.rhs.*);
                try self.hir.instructions.append(self.allocator, .{ .negate = unary_operation.source_loc });
            },

            .bang => {
                try self.generateExpr(unary_operation.rhs.*);
                try self.hir.instructions.append(self.allocator, .{ .bool_not = unary_operation.source_loc });
            },

            .tilde => {
                try self.generateExpr(unary_operation.rhs.*);
                try self.hir.instructions.append(self.allocator, .{ .bit_not = unary_operation.source_loc });
            },

            .ampersand => {
                if (unary_operation.rhs.* == .subscript) {
                    try self.generateExpr(unary_operation.rhs.subscript.target.*);
                    try self.generateExpr(unary_operation.rhs.subscript.index.*);

                    try self.hir.instructions.append(self.allocator, .{ .offset = unary_operation.rhs.subscript.source_loc });
                } else {
                    try self.generateExpr(unary_operation.rhs.*);

                    try self.hir.instructions.append(self.allocator, .{ .reference = unary_operation.source_loc });
                }
            },

            .star => {
                try self.generateExpr(unary_operation.rhs.*);
                try self.hir.instructions.append(self.allocator, .{ .read = unary_operation.source_loc });
            },
        }
    }

    fn generateBinaryOperationExpr(self: *Generator, binary_operation: Ast.Node.Expr.BinaryOperation) Error!void {
        switch (binary_operation.operator) {
            .plus => {
                try self.generateExpr(binary_operation.lhs.*);
                try self.generateExpr(binary_operation.rhs.*);

                try self.hir.instructions.append(self.allocator, .{ .add = binary_operation.source_loc });
            },

            .minus => {
                try self.generateExpr(binary_operation.lhs.*);
                try self.generateExpr(binary_operation.rhs.*);

                try self.hir.instructions.append(self.allocator, .{ .sub = binary_operation.source_loc });
            },

            .star => {
                try self.generateExpr(binary_operation.lhs.*);
                try self.generateExpr(binary_operation.rhs.*);

                try self.hir.instructions.append(self.allocator, .{ .mul = binary_operation.source_loc });
            },

            .forward_slash => {
                try self.generateExpr(binary_operation.lhs.*);
                try self.generateExpr(binary_operation.rhs.*);

                try self.hir.instructions.append(self.allocator, .{ .div = binary_operation.source_loc });
            },

            .equal_sign => {
                try self.generateExpr(binary_operation.rhs.*);
                try self.hir.instructions.append(self.allocator, .duplicate);

                if (binary_operation.lhs.* == .unary_operation and binary_operation.lhs.unary_operation.operator == .star) {
                    try self.generateExpr(binary_operation.lhs.unary_operation.rhs.*);

                    try self.hir.instructions.append(self.allocator, .{ .write = binary_operation.source_loc });
                } else if (binary_operation.lhs.* == .identifier) {
                    try self.hir.instructions.append(self.allocator, .{ .set = binary_operation.lhs.identifier.name });
                } else {
                    self.error_info = .{ .message = "expected an identifier or a pointer dereference", .source_loc = binary_operation.lhs.getSourceLoc() };

                    return error.UnexpectedExpression;
                }
            },

            .less_than => {
                try self.generateExpr(binary_operation.lhs.*);
                try self.generateExpr(binary_operation.rhs.*);

                try self.hir.instructions.append(self.allocator, .{ .lt = binary_operation.source_loc });
            },

            .greater_than => {
                try self.generateExpr(binary_operation.lhs.*);
                try self.generateExpr(binary_operation.rhs.*);

                try self.hir.instructions.append(self.allocator, .{ .gt = binary_operation.source_loc });
            },

            .double_less_than => {
                try self.generateExpr(binary_operation.lhs.*);
                try self.generateExpr(binary_operation.rhs.*);

                try self.hir.instructions.append(self.allocator, .{ .shl = binary_operation.source_loc });
            },

            .double_greater_than => {
                try self.generateExpr(binary_operation.lhs.*);
                try self.generateExpr(binary_operation.rhs.*);

                try self.hir.instructions.append(self.allocator, .{ .shr = binary_operation.source_loc });
            },

            .ampersand => {
                try self.generateExpr(binary_operation.lhs.*);
                try self.generateExpr(binary_operation.rhs.*);

                try self.hir.instructions.append(self.allocator, .{ .bit_and = binary_operation.source_loc });
            },

            .pipe => {
                try self.generateExpr(binary_operation.lhs.*);
                try self.generateExpr(binary_operation.rhs.*);

                try self.hir.instructions.append(self.allocator, .{ .bit_or = binary_operation.source_loc });
            },

            .caret => {
                try self.generateExpr(binary_operation.lhs.*);
                try self.generateExpr(binary_operation.rhs.*);

                try self.hir.instructions.append(self.allocator, .{ .bit_xor = binary_operation.source_loc });
            },

            .double_equal_sign, .bang_equal_sign => {
                try self.generateExpr(binary_operation.lhs.*);
                try self.generateExpr(binary_operation.rhs.*);

                try self.hir.instructions.append(self.allocator, .{ .eql = binary_operation.source_loc });

                if (binary_operation.operator == .bang_equal_sign) {
                    try self.hir.instructions.append(self.allocator, .{ .bool_not = binary_operation.source_loc });
                }
            },
        }
    }

    fn generateSubscript(self: *Generator, subscript: Ast.Node.Expr.Subscript) Error!void {
        try self.generateExpr(subscript.target.*);
        try self.generateExpr(subscript.index.*);

        try self.hir.instructions.append(self.allocator, .{ .offset = subscript.source_loc });
        try self.hir.instructions.append(self.allocator, .{ .read = subscript.source_loc });
    }

    fn generateCallExpr(self: *Generator, call: Ast.Node.Expr.Call) Error!void {
        var i: usize = call.arguments.len;

        while (i > 0) {
            i -= 1;

            const argument = call.arguments[i];

            try self.generateExpr(argument);
        }

        try self.generateExpr(call.callable.*);

        try self.hir.instructions.append(self.allocator, .{ .call = .{ .arguments_count = call.arguments.len, .source_loc = call.source_loc } });
    }
};
