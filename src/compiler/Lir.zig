//! Low Intermediate Representation.
//!
//! An analyzed and checked stack-based intermediate representation lowered from `Hir`.
//! Last intermediate representation to be used before lowering to machine code.

const std = @import("std");

const Symbol = @import("Symbol.zig");
const Type = Symbol.Type;

instructions: std.ArrayListUnmanaged(Instruction) = .{},

pub const Instruction = union(enum) {
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
    read: Type,
    /// Calculate the pointer of an element in a "size many" pointer
    get_element_ptr: Type,
    /// Add two integers or floats on the top of the stack
    add,
    /// Subtract two integers or floats on the top of the stack
    sub,
    /// Multiply two integers or floats on the top of the stack
    mul,
    /// Divide two floats on the top of the stack
    fdiv,
    /// Same as `fdiv` but for signed integers
    sdiv,
    /// Same as `fdiv` but for unsigned integers
    udiv,
    /// Compare between two integers on the stack
    icmp: ICmp,
    /// Compare between two floats on the stack
    fcmp: FCmp,
    /// Shift to left the bits of lhs using rhs offset
    shl,
    /// Shift to right the bits of lhs using rhs offset
    shr,
    /// Place a machine-specific assembly in the output
    assembly: Assembly,
    /// Declare function parameters
    parameters: []const Symbol,
    /// Call a function pointer on top of the stack
    call: Type.Function,
    /// Declare a function
    function: Function,
    /// Declare a variable using the specified name and type
    variable: Symbol,
    /// Same as `variable` but the variable is external
    external: Symbol,
    /// Set a variable with a value on top of the stack
    set: []const u8,
    /// Get a value of a variable
    get: []const u8,
    /// Get a pointer to variable
    get_ptr: []const u8,
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

    pub const Function = struct {
        name: []const u8,
        type: Type,
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
    };

    pub const ICmp = enum {
        slt,
        sgt,
        ult,
        ugt,
        eql,
    };

    pub const FCmp = enum {
        lt,
        gt,
        eql,
    };
};
