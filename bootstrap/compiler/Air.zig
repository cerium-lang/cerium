//! Analyzed Intermediate Representation.
//!
//! An analyzed and checked stack-based intermediate representation lowered from `Sir`.
//! Last intermediate representation to be used before lowering to machine code.

const std = @import("std");

const Symbol = @import("Symbol.zig");
const Type = Symbol.Type;

const Air = @This();

pub const SymbolMaybeExported = struct {
    symbol: Symbol,
    exported: bool,
};

instructions: std.ArrayListUnmanaged(Instruction) = .{},

pub const Instruction = union(enum) {
    /// No operation
    nop,
    /// Duplicate the top of the stack
    duplicate,
    /// Reverse the stack into nth depth
    reverse: u32,
    /// Pop the top of the stack
    pop,
    /// Push a string onto the stack
    string: []const u8,
    /// Push an integer onto the stack
    int: i128,
    /// Push a float onto the stack
    float: f64,
    /// Push a boolean onto the stack
    boolean: bool,
    /// Negate an integer or float on the top of the stack
    negate,
    /// Reverse a boolean from true to false and from false to true
    bool_not,
    /// Perform bitwise NOT operation on the bits of rhs (Which is to reverse its bits representation)
    bit_not,
    /// Perform bitwise AND operation on the bits of lhs and rhs
    bit_and,
    /// Perform bitwise OR operation on the bits of lhs and rhs
    bit_or,
    /// Perform bitwise XOR operation on the bits of lhs and rhs
    bit_xor,
    /// Override the data that the pointer is pointing to
    write,
    /// Read the data that the pointer is pointing to
    read,
    /// Add two integers or floats on the top of the stack
    add,
    /// Subtract two integers or floats on the top of the stack
    sub,
    /// Multiply two integers or floats on the top of the stack
    mul,
    /// Divide two integers or floats on the top of the stack
    div,
    /// Remainder of two integers or floats on the top of the stack
    rem,
    /// Compare between two integers or floats on the stack and check for order (in this case, lhs less than rhs)
    lt,
    /// Compare between two integers or floats on the stack and check for order (in this case, lhs greater than rhs)
    gt,
    /// Compare between two values on the stack and check for equality
    eql,
    /// Shift to left the bits of lhs using rhs offset
    shl,
    /// Shift to right the bits of lhs using rhs offset
    shr,
    /// Cast a value to a different type
    cast: Type,
    /// Place a machine-specific assembly in the output
    assembly: Assembly,
    /// Declare function parameters
    parameters: []const Symbol,
    /// Call a function pointer on top of the stack
    call: usize,
    /// Declare a function
    function: SymbolMaybeExported,
    /// Declare a variable using the specified name and type
    variable: SymbolMaybeExported,
    /// Same as `variable` but the variable is external
    external: Symbol,
    /// Get a pointer to variable
    get_variable_ptr: []const u8,
    /// Calculate the pointer of an element in a "size many" pointer
    get_element_ptr,
    /// Calculate the pointer of a field in a struct pointer
    get_field_ptr: u32,
    /// Start a new block
    block: u32,
    /// Unconditionally branch to a block
    br: u32,
    /// Conditionally branch to a block, condition is on the stack
    cond_br: CondBr,
    /// Switch on value to branch to a block
    @"switch": Switch,
    /// Start a new scope
    start_scope,
    /// End a scope
    end_scope,
    /// Return out of the function with a value on the stack
    ret,
    /// Return out of the function without a value
    ret_void,

    pub const Assembly = struct {
        content: []const u8,
        output_constraint: ?OutputConstraint,
        input_constraints: []const []const u8,
        clobbers: []const []const u8,

        pub const OutputConstraint = struct {
            register: []const u8,
            type: Type,
        };
    };

    pub const CondBr = struct {
        true_id: u32,
        false_id: u32,
    };

    pub const Switch = struct {
        case_block_ids: []const u32,
        else_block_id: u32,
    };
};

pub const passes = struct {
    pub fn removeRedundantDeclarations(allocator: std.mem.Allocator, airs: []Air) std.mem.Allocator.Error!void {
        var declarations: std.StringHashMapUnmanaged(struct { bool, []Air.Instruction }) = .{};
        defer declarations.deinit(allocator);

        for (airs) |air| {
            var i: usize = 0;

            while (i < air.instructions.items.len) : (i += 1) {
                const air_instruction = air.instructions.items[i];

                switch (air_instruction) {
                    .function => |symbol_maybe_exported| {
                        const start = i;
                        var end = i + 1;

                        var scope_depth: usize = 0;

                        for (air.instructions.items[end..]) |function_instruction| {
                            end += 1;
                            i += 1;

                            switch (function_instruction) {
                                .start_scope => scope_depth += 1,
                                .end_scope => {
                                    scope_depth -= 1;
                                    if (scope_depth == 0) break;
                                },

                                else => {},
                            }
                        }

                        try declarations.put(
                            allocator,
                            symbol_maybe_exported.symbol.name.buffer,
                            .{
                                symbol_maybe_exported.exported,
                                air.instructions.items[start..end],
                            },
                        );
                    },

                    .variable => |symbol_maybe_exported| {
                        if (symbol_maybe_exported.symbol.linkage != .global) continue;

                        const start = i - 1;
                        const end = i + 1;

                        try declarations.put(
                            allocator,
                            symbol_maybe_exported.symbol.name.buffer,
                            .{
                                symbol_maybe_exported.exported,
                                air.instructions.items[start..end],
                            },
                        );
                    },

                    .external => |symbol| {
                        const start = i;
                        const end = i + 1;

                        try declarations.put(allocator, symbol.name.buffer, .{ false, air.instructions.items[start..end] });
                    },

                    else => {},
                }
            }
        }

        for (airs) |air|
            for (air.instructions.items) |air_instruction|
                switch (air_instruction) {
                    .get_variable_ptr => |name| _ = declarations.remove(name),

                    else => {},
                };

        var declaration_iterator = declarations.valueIterator();

        while (declaration_iterator.next()) |declaration| {
            const exported, const air_instructions = declaration.*;

            if (exported) continue;

            for (air_instructions) |*air_instruction|
                air_instruction.* = .nop;
        }
    }
};
