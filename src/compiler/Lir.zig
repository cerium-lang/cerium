//! Low Intermediate Representation.
//!
//! An analyzed and checked stack-based intermediate representation lowered from `Hir`.
//! Returned from `Sema` and is the last intermediate representation to be used before lowering to machine code.

const std = @import("std");

const Symbol = @import("Symbol.zig");
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
    /// Declare a function parameter, contains the symbol so the backend knows how to store it on the stack
    function_parameter: struct { usize, Symbol },
    /// Call a specific function pointer on the stack with the specified type
    call: Type.Data.Function,
    /// Declare a variable using the specified name and type
    variable: Symbol,
    /// Set a stack value using the specified name
    set: []const u8,
    /// Get a value using the specified name
    get: []const u8,
    /// Get pointer of a value using specified name
    get_ptr: []const u8,
    /// Push a string onto the stack
    string: []const u8,
    /// Push an integer onto the stack
    int: i128,
    /// Push a float onto the stack
    float: f64,
    /// Push a boolean onto the stack
    boolean: bool,
    /// Negate an integer or float
    negate,
    /// Reverse a boolean from true to false and from false to true
    bool_not,
    /// Read the data that the pointer is pointing to
    read: Type,
    /// Override the data that the pointer is pointing to
    write,
    /// Add two integers or floats on the top of the stack
    add,
    /// Subtract two integers or floats on the top of the stack
    sub,
    /// Multiply two integers or floats on the top of the stack
    mul,
    /// Divide two integers or floats on the top of the stack
    div,
    /// Compare between two values on the stack and check for equality
    eql,
    /// Pop a value from the stack
    pop,
    /// Place a machine-specific assembly in the output
    assembly: []const u8,
    /// Return to the parent block
    @"return",
};
