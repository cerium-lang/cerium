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
    /// Place a machine-specific global assembly in the output
    /// (This is a special case of `inline_assembly` that is only used when the assembly is outside of a function)
    global_assembly: []const u8,
    /// Place a machine-specific inline assembly in the output
    inline_assembly: InlineAssembly,
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

    pub const InlineAssembly = struct {
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
    pub const redundancy = struct {
        const Function = struct {
            exported: bool,
            callers: usize = 0,
            callees: std.StringArrayHashMapUnmanaged(void) = .{},
            air_instructions: []Air.Instruction,
        };

        pub fn removeRedundantDeclarations(allocator: std.mem.Allocator, airs: []Air) std.mem.Allocator.Error!void {
            var functions: std.StringArrayHashMapUnmanaged(Function) = .{};

            defer {
                for (functions.values()) |*function|
                    if (function.callers != 0)
                        function.callees.deinit(allocator);

                functions.deinit(allocator);
            }

            var variables: std.StringArrayHashMapUnmanaged([]Air.Instruction) = .{};
            defer variables.deinit(allocator);

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

                            try functions.put(allocator, symbol_maybe_exported.symbol.name.buffer, .{
                                .exported = symbol_maybe_exported.exported,
                                .air_instructions = air.instructions.items[start..end],
                            });
                        },

                        .variable => |symbol_maybe_exported| {
                            if (symbol_maybe_exported.symbol.linkage != .global or symbol_maybe_exported.exported) continue;

                            const start = i - 1;
                            const end = i + 1;

                            try variables.put(
                                allocator,
                                symbol_maybe_exported.symbol.name.buffer,
                                air.instructions.items[start..end],
                            );
                        },

                        else => {},
                    }
                }
            }

            for (functions.values()) |*function|
                for (function.air_instructions) |air_instruction|
                    switch (air_instruction) {
                        .get_variable_ptr => |name| {
                            if (functions.getPtr(name)) |other_function| {
                                other_function.callers += 1;
                                try function.callees.put(allocator, name, {});
                            } else {
                                _ = variables.swapRemove(name);
                            }
                        },

                        else => {},
                    };

            for (functions.values()) |*function|
                checkRedundantFunction(allocator, &functions, function);

            for (variables.values()) |air_instructions| {
                for (air_instructions) |*air_instruction| {
                    air_instruction.* = .nop;
                }
            }
        }

        fn checkRedundantFunction(allocator: std.mem.Allocator, functions: *std.StringArrayHashMapUnmanaged(Function), function: *Function) void {
            if (function.exported) return;

            if (function.callers == 0) {
                for (function.callees.keys()) |name| {
                    const other_function = functions.getPtr(name).?;

                    other_function.callers -= 1;

                    checkRedundantFunction(allocator, functions, other_function);
                }

                function.callees.deinit(allocator);

                for (function.air_instructions) |*air_instruction|
                    air_instruction.* = .nop;
            }
        }
    };

    pub const format = struct {
        pub fn print(writer: anytype, airs: []Air) !void {
            var scope_depth: usize = 0;

            for (airs) |air| {
                for (air.instructions.items) |instruction| {
                    try writer.writeByteNTimes('\t', if (instruction == .end_scope) scope_depth - 1 else scope_depth);

                    switch (instruction) {
                        .nop => {},

                        .duplicate => try writer.writeAll("duplicate\n"),
                        .pop => try writer.writeAll("pop\n"),

                        .reverse => |n| try writer.print("reverse {}\n", .{n}),

                        .string => |string| {
                            try writer.writeAll("string \"");
                            try std.zig.stringEscape(string, "", .{}, writer);
                            try writer.writeAll("\"\n");
                        },

                        .int => |int| try writer.print("int {}\n", .{int}),

                        .float => |float| try writer.print("float {}\n", .{float}),

                        .boolean => |boolean| try writer.print("boolean {}\n", .{boolean}),

                        .negate => try writer.writeAll("negate\n"),
                        .bool_not => try writer.writeAll("bool_not\n"),
                        .bit_not => try writer.writeAll("bit_not\n"),
                        .bit_and => try writer.writeAll("bit_and\n"),
                        .bit_or => try writer.writeAll("bit_or\n"),
                        .bit_xor => try writer.writeAll("bit_xor\n"),
                        .write => try writer.writeAll("write\n"),
                        .read => try writer.writeAll("read\n"),
                        .add => try writer.writeAll("add\n"),
                        .sub => try writer.writeAll("sub\n"),
                        .mul => try writer.writeAll("mul\n"),
                        .div => try writer.writeAll("div\n"),
                        .rem => try writer.writeAll("rem\n"),
                        .lt => try writer.writeAll("lt\n"),
                        .gt => try writer.writeAll("gt\n"),
                        .eql => try writer.writeAll("eql\n"),
                        .shl => try writer.writeAll("shl\n"),
                        .shr => try writer.writeAll("shr\n"),

                        .cast => |to_type| try writer.print("cast to {}\n", .{to_type}),

                        .global_assembly => |global_assembly| {
                            try writer.writeAll("\nglobal_assembly:\n");
                            try writer.writeByteNTimes('\t', scope_depth + 1);
                            try writer.writeAll("content:\n");

                            var content_iterator = std.mem.splitScalar(u8, global_assembly, '\n');
                            while (content_iterator.next()) |line| {
                                try writer.writeByteNTimes('\t', scope_depth + 2);
                                try writer.writeByte('"');
                                try std.zig.stringEscape(line, "", .{}, writer);
                                try writer.writeAll("\"\n");
                            }
                        },

                        .inline_assembly => |inline_assembly| {
                            try writer.writeAll("inline_assembly:\n");
                            try writer.writeByteNTimes('\t', scope_depth + 1);
                            try writer.writeAll("content:\n");

                            var content_iterator = std.mem.splitScalar(u8, inline_assembly.content, '\n');
                            while (content_iterator.next()) |line| {
                                try writer.writeByteNTimes('\t', scope_depth + 2);
                                try writer.writeByte('"');
                                try std.zig.stringEscape(line, "", .{}, writer);
                                try writer.writeAll("\"\n");
                            }

                            try writer.writeByteNTimes('\t', scope_depth + 1);
                            try writer.writeAll("input constraints:\n");

                            for (inline_assembly.input_constraints) |constraint| {
                                try writer.writeByteNTimes('\t', scope_depth + 2);
                                try writer.writeByte('"');
                                try std.zig.stringEscape(constraint, "", .{}, writer);
                                try writer.writeAll("\"\n");
                            }

                            try writer.writeByteNTimes('\t', scope_depth + 1);
                            try writer.writeAll("output constraint:\n");

                            if (inline_assembly.output_constraint) |output_constraint| {
                                try writer.writeByteNTimes('\t', scope_depth + 2);
                                try writer.writeByte('"');
                                try std.zig.stringEscape(output_constraint.register, "", .{}, writer);
                                try writer.print("\" {}\n", .{output_constraint.type});
                            } else {
                                try writer.writeByteNTimes('\t', scope_depth + 2);
                                try writer.writeAll("none\n");
                            }

                            try writer.writeByteNTimes('\t', scope_depth + 1);
                            try writer.writeAll("clobbers:\n");

                            for (inline_assembly.clobbers) |clobber| {
                                try writer.writeByteNTimes('\t', scope_depth + 2);
                                try writer.writeByte('"');
                                try std.zig.stringEscape(clobber, "", .{}, writer);
                                try writer.writeAll("\"\n");
                            }
                        },

                        .parameters => |parameters| {
                            try writer.writeAll("parameters:\n");

                            for (parameters) |parameter| {
                                try writer.writeByteNTimes('\t', scope_depth + 1);
                                try writer.print("{s} {}\n", .{ parameter.name.buffer, parameter.type });
                            }
                        },

                        .call => |id| try writer.print("call {}\n", .{id}),

                        .function => |symbol_maybe_exported| try writer.print("\nfunction {s} ({s}, type: {}):\n", .{
                            symbol_maybe_exported.symbol.name.buffer,
                            if (symbol_maybe_exported.exported) "exported" else "not exported",
                            symbol_maybe_exported.symbol.type,
                        }),

                        .variable => |symbol_maybe_exported| try writer.print("variable {s} ({s}, type: {})\n", .{
                            symbol_maybe_exported.symbol.name.buffer,
                            if (symbol_maybe_exported.exported) "exported" else "not exported",
                            symbol_maybe_exported.symbol.type,
                        }),

                        .external => |symbol| try writer.print("external {s} (type: {})\n", .{ symbol.name.buffer, symbol.type }),

                        .get_variable_ptr => |name| try writer.print("get_variable_ptr {s}\n", .{name}),
                        .get_element_ptr => try writer.writeAll("get_element_ptr\n"),
                        .get_field_ptr => |n| try writer.print("get_field_ptr {}\n", .{n}),

                        .block => |id| try writer.print("block {}:\n", .{id}),

                        .br => |id| try writer.print("br {}\n", .{id}),

                        .cond_br => |cond_br| {
                            try writer.writeAll("cond_br:\n");
                            try writer.writeByteNTimes('\t', scope_depth + 1);
                            try writer.print("true {}\n", .{cond_br.true_id});
                            try writer.writeByteNTimes('\t', scope_depth + 1);
                            try writer.print("false {}\n", .{cond_br.false_id});
                        },

                        .@"switch" => |@"switch"| try writer.print("switch {}\n", .{@"switch"}),

                        .start_scope => {
                            scope_depth += 1;
                            try writer.writeAll("{\n");
                        },

                        .end_scope => {
                            scope_depth -= 1;
                            try writer.writeAll("}\n");
                        },

                        .ret => try writer.writeAll("ret\n"),
                        .ret_void => try writer.writeAll("ret_void\n"),
                    }
                }
            }
        }
    };
};
