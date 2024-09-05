//! Low Intermediate Representation.
//!
//! An analyzed and checked stack-based intermediate representation lowered from `Hir`.
//! Returned from `Sema` and is the last intermediate representation to be used before lowering to machine code.

const std = @import("std");

const Ast = @import("Ast.zig");
const Type = @import("Type.zig");

const Lir = @This();

instructions: std.ArrayListUnmanaged(Instruction) = .{},

pub const Instruction = union(enum) {
    /// Start a labeled block
    label: []const u8,
    /// Start a function block
    function_proluge,
    /// End a function block,
    function_epilogue,
    /// Set a stack value using the specified name
    set: []const u8,
    /// Get a stack value using the specified name
    get: []const u8,
    /// Push a string literal onto the stack
    string: []const u8,
    /// Push an integer literal onto the stack
    int: i128,
    /// Push a float literal onto the stack
    float: f64,
    /// Negate an integer or float
    negate,
    /// Add two integers or floats on the top of the stack
    add,
    /// Subtract two integers or floats on the top of the stack
    sub,
    /// Pop a value from the stack
    pop,
    /// Place a machine-specific assembly in the output
    assembly: []const u8,
    /// Return to the parent block
    @"return",
};
