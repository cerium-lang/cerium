//! High Intermediate Representation.
//!
//! An unchecked stack-based intermediate representation lowered from `Ast`.
//! Passed to `Sema` which checks all the instructions and types to be valid and lowers it down to `Lir`,
//! And then `Lir` gets lowered down to machine code.

const std = @import("std");

const Ast = @import("Ast.zig");
const Symbol = @import("Symbol.zig");
const Type = @import("Type.zig");

const Hir = @This();

blocks: std.ArrayListUnmanaged(Block) = .{},

pub const Block = struct {
    tag: Tag,

    label: Ast.Name,

    maybe_function: ?Ast.Node.Stmt.FunctionDeclaration = null,

    instructions: std.ArrayListUnmanaged(Instruction) = .{},

    pub const Tag = enum {
        basic,
        data,
    };

    pub const Instruction = union(enum) {
        /// Start a function, pass the function declaration node to enable more checks
        function_proluge,
        /// End a function
        function_epilogue,
        /// Declare a function parameter
        function_parameter,
        /// Call a specific function pointer on the stack with the specified argument count
        call: struct { usize, Ast.SourceLoc },
        /// Declare a constant using the specified name and type
        constant: Symbol,
        /// Same as `constant` but the type is unknown at the point of declaration
        constant_infer: Symbol,
        /// Declare a variable using the specified name and type
        variable: Symbol,
        /// Same as `variable` but the type is unknown at the point of declaration
        variable_infer: Symbol,
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
        /// Add two integers or floats on the top of the stack
        add: Ast.SourceLoc,
        /// Subtract two integers or floats on the top of the stack
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
        /// Pop a value from the stack
        pop,
        /// Return to the parent block
        @"return",
    };
};

pub const Generator = struct {
    allocator: std.mem.Allocator,

    hir: Hir = .{},

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
    } || std.mem.Allocator.Error;

    pub fn init(allocator: std.mem.Allocator) Generator {
        return Generator{
            .allocator = allocator,
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
            .function_declaration => try self.generateFunctionDeclarationStmt(stmt.function_declaration),

            .variable_declaration => try self.generateVariableDeclarationStmt(stmt.variable_declaration),

            .@"return" => try self.generateReturnStmt(stmt.@"return"),
        }
    }

    fn generateFunctionDeclarationStmt(self: *Generator, function: Ast.Node.Stmt.FunctionDeclaration) Error!void {
        if (self.maybe_hir_block != null) {
            self.error_info = .{ .message = "cannot declare functions inside other functions", .source_loc = function.prototype.name.source_loc };

            return error.UnexpectedStatement;
        }

        const new_hir_block = try self.hir.blocks.addOne(self.allocator);

        new_hir_block.* = .{
            .tag = .basic,
            .label = function.prototype.name,
            .maybe_function = function,
        };

        try new_hir_block.instructions.append(self.allocator, .function_proluge);

        for (0..function.prototype.parameters.len) |_| {
            try new_hir_block.instructions.append(self.allocator, .function_parameter);
        }

        self.maybe_hir_block = new_hir_block;
        defer self.maybe_hir_block = null;

        for (function.body) |node| {
            try self.generateNode(node);
        }

        try new_hir_block.instructions.append(self.allocator, .function_epilogue);

        try new_hir_block.instructions.append(self.allocator, .@"return");
    }

    fn emitVariable(allocator: std.mem.Allocator, hir_block: *Hir.Block, variable: Ast.Node.Stmt.VariableDeclaration, linkage: Symbol.Linkage) Error!void {
        try hir_block.instructions.append(
            allocator,
            if (variable.type) |@"type"|
                if (variable.is_const)
                    .{
                        .constant = .{
                            .name = variable.name,
                            .type = @"type",
                            .linkage = linkage,
                        },
                    }
                else
                    .{
                        .variable = .{
                            .name = variable.name,
                            .type = @"type",
                            .linkage = linkage,
                        },
                    }
            else if (variable.is_const)
                .{
                    .constant_infer = .{
                        .name = variable.name,
                        .type = .{ .tag = .void },
                        .linkage = linkage,
                    },
                }
            else
                .{
                    .variable_infer = .{
                        .name = variable.name,
                        .type = .{ .tag = .void },
                        .linkage = linkage,
                    },
                },
        );
    }

    fn generateVariableDeclarationStmt(self: *Generator, variable: Ast.Node.Stmt.VariableDeclaration) Error!void {
        if (self.maybe_hir_block) |hir_block| {
            try self.generateExpr(variable.value);

            try emitVariable(self.allocator, hir_block, variable, .local);

            try hir_block.instructions.append(self.allocator, .{ .set = variable.name });
        } else {
            const new_hir_block = try self.hir.blocks.addOne(self.allocator);
            new_hir_block.* = .{ .tag = .data, .label = variable.name };

            self.maybe_hir_block = new_hir_block;
            defer self.maybe_hir_block = null;

            try self.generateExpr(variable.value);

            try emitVariable(self.allocator, new_hir_block, variable, .global);

            try new_hir_block.instructions.append(self.allocator, .{ .set = variable.name });
        }
    }

    fn generateReturnStmt(self: *Generator, @"return": Ast.Node.Stmt.Return) Error!void {
        if (self.maybe_hir_block) |hir_block| {
            try self.generateExpr(@"return".value);

            try hir_block.instructions.append(self.allocator, .function_epilogue);

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
        for (assembly.input_constraints) |input_constraint| {
            try self.generateExpr(input_constraint.value.*);
        }

        try self.maybe_hir_block.?.instructions.append(self.allocator, .{ .assembly = assembly });
    }

    fn generateUnaryOperationExpr(self: *Generator, unary_operation: Ast.Node.Expr.UnaryOperation) Error!void {
        try self.generateExpr(unary_operation.rhs.*);

        switch (unary_operation.operator) {
            .minus => {
                try self.maybe_hir_block.?.instructions.append(self.allocator, .{ .negate = unary_operation.source_loc });
            },

            .bang => {
                try self.maybe_hir_block.?.instructions.append(self.allocator, .{ .bool_not = unary_operation.source_loc });
            },

            .tilde => {
                try self.maybe_hir_block.?.instructions.append(self.allocator, .{ .bit_not = unary_operation.source_loc });
            },

            .ampersand => {
                try self.maybe_hir_block.?.instructions.append(self.allocator, .{ .reference = unary_operation.source_loc });
            },

            .star => {
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

    fn generateCallExpr(self: *Generator, call: Ast.Node.Expr.Call) Error!void {
        for (call.arguments) |argument| {
            try self.generateExpr(argument);
        }

        try self.generateExpr(call.callable.*);

        try self.maybe_hir_block.?.instructions.append(self.allocator, .{ .call = .{ call.arguments.len, call.source_loc } });
    }
};
