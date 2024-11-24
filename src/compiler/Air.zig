//! Analyzed Intermediate Representation.
//!
//! An analyzed and checked stack-based intermediate representation lowered from `Sir`.
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
    function: Symbol,
    /// Declare a variable using the specified name and type
    variable: Symbol,
    /// Same as `variable` but the variable is external
    external: Symbol,
    /// Get a pointer to variable
    get_variable_ptr: []const u8,
    /// Calculate the pointer of an element in a "size many" pointer
    get_element_ptr,
    /// Calculate the pointer of a field in a struct pointer
    get_field_ptr: u32,
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
};
