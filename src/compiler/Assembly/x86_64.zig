const std = @import("std");

const Assembly = @import("../Assembly.zig");
const Lir = @import("../Lir.zig");
const Symbol = @import("../Symbol.zig");
const Type = @import("../Type.zig");

const x86_64 = @This();

allocator: std.mem.Allocator,

assembly: Assembly = .{},

lir: Lir,

variables: Variable.Table,

stack: std.ArrayListUnmanaged(StackAllocation) = .{},
stack_offsets: std.ArrayListUnmanaged(usize) = .{},
stack_alignment: usize = 16,

string_literals_index: usize = 0,
floating_points_index: usize = 0,

pub const Variable = struct {
    stack_index: usize,
    type: Type,
    linkage: Symbol.Linkage,

    pub const Table = struct {
        allocator: std.mem.Allocator,

        original: std.StringHashMapUnmanaged(Variable) = .{},
        modified: std.StringHashMapUnmanaged(Variable) = .{},

        pub fn init(allocator: std.mem.Allocator) Table {
            return Table{ .allocator = allocator };
        }

        pub fn put(self: *Table, name: []const u8, variable: Variable) std.mem.Allocator.Error!void {
            try self.modified.put(self.allocator, name, variable);

            if (variable.linkage == .global) {
                try self.original.put(self.allocator, name, variable);
            }
        }

        pub fn get(self: Table, name: []const u8) ?Variable {
            if (self.modified.get(name)) |variable| {
                return variable;
            } else if (self.original.get(name)) |variable| {
                return variable;
            }

            return null;
        }

        pub fn getPtr(self: *Table, name: []const u8) ?*Variable {
            if (self.modified.getPtr(name)) |variable| {
                return variable;
            } else if (self.original.getPtr(name)) |variable| {
                return variable;
            }

            return null;
        }

        pub fn reset(self: *Table) void {
            self.modified.clearRetainingCapacity();
        }
    };
};

const StackAllocation = struct {
    is_floating_point: bool,
};

pub fn init(allocator: std.mem.Allocator, lir: Lir) x86_64 {
    return x86_64{
        .allocator = allocator,
        .lir = lir,
        .variables = Variable.Table.init(allocator),
    };
}

pub fn deinit(self: *x86_64) void {
    self.assembly.deinit(self.allocator);
    self.stack.clearAndFree(self.allocator);
    self.stack_offsets.clearAndFree(self.allocator);
}

pub const Error = std.mem.Allocator.Error;

pub fn render(self: *x86_64) Error!void {
    const text_section_writer = self.assembly.text_section.writer(self.allocator);
    const data_section_writer = self.assembly.data_section.writer(self.allocator);
    const rodata_section_writer = self.assembly.rodata_section.writer(self.allocator);

    for (self.lir.instructions.items, 0..) |instruction, i| {
        switch (instruction) {
            .label => |name| {
                const section_writer = if (self.lir.instructions.items[i + 1].variable.type.getFunction() == null) data_section_writer else text_section_writer;

                try section_writer.print(".global {s}\n", .{name});
                try section_writer.print("{s}:\n", .{name});
            },

            .function_proluge => {
                try text_section_writer.writeAll("\tpushq %rbp\n");
                try text_section_writer.writeAll("\tmovq %rsp, %rbp\n");

                try self.stack_offsets.append(self.allocator, 0);
            },

            .function_epilogue => {
                if (self.stack.items.len != 0) {
                    try self.popRegister(text_section_writer, "ax");
                }

                try text_section_writer.writeAll("\tpopq %rbp\n");

                self.variables.reset();
                self.stack.clearRetainingCapacity();
                self.stack_offsets.clearRetainingCapacity();
            },

            .function_parameter => |function_parameter| {
                const function_parameter_index, const function_parameter_symbol = function_parameter;

                const is_floating_point = function_parameter_symbol.type.isFloat();

                if (is_floating_point) {
                    try text_section_writer.print("\tmovq {}(%rbp), %xmm8\n", .{(function_parameter_index + 1) * self.stack_alignment});
                } else {
                    try text_section_writer.print("\tmovq {}(%rbp), %r8\n", .{(function_parameter_index + 1) * self.stack_alignment});
                }

                try self.pushRegister(text_section_writer, "8", .{ .is_floating_point = is_floating_point });

                try self.variables.put(
                    function_parameter_symbol.name.buffer,
                    .{
                        .stack_index = self.stack.items.len - 1,
                        .type = function_parameter_symbol.type,
                        .linkage = function_parameter_symbol.linkage,
                    },
                );
            },

            .call => |function| {
                try self.popRegister(text_section_writer, "8");

                for (function.parameters, 0..) |parameter, j| {
                    try self.popRegister(text_section_writer, "9");

                    if (parameter.isFloat()) {
                        try text_section_writer.print("\tmovq %xmm9, {}(%rsp)\n", .{j * self.stack_alignment});
                    } else {
                        try text_section_writer.print("\tmovq %r9, {}(%rsp)\n", .{j * self.stack_alignment});
                    }
                }

                try text_section_writer.writeAll("\tcallq *%r8\n");

                try self.pushRegister(text_section_writer, "ax", .{ .is_floating_point = function.return_type.isFloat() });
            },

            .variable => |symbol| {
                try self.variables.put(
                    symbol.name.buffer,
                    .{
                        .stack_index = 0,
                        .type = symbol.type,
                        .linkage = symbol.linkage,
                    },
                );
            },

            .set => |name| {
                const variable = self.variables.getPtr(name).?;

                if (variable.linkage == .global) {
                    const stack_allocation = self.stack.items[variable.stack_index];

                    try self.popRegister(text_section_writer, "8");

                    try copyToData(text_section_writer, "8", stack_allocation, name);
                } else if (variable.stack_index == 0) {
                    variable.stack_index = self.stack.items.len - 1;
                } else {
                    const stack_allocation = self.stack.items[variable.stack_index];

                    try self.popRegister(text_section_writer, "8");

                    try copyToStack(text_section_writer, "8", stack_allocation, self.stack_offsets.items[variable.stack_index + 1]);
                }
            },

            .get => |name| {
                const variable = self.variables.get(name).?;

                switch (variable.linkage) {
                    .local => {
                        const stack_allocation = self.stack.items[variable.stack_index];

                        try copyFromStack(text_section_writer, "8", stack_allocation, self.stack_offsets.items[variable.stack_index + 1]);

                        try self.pushRegister(text_section_writer, "8", stack_allocation);
                    },

                    .global => {
                        const stack_allocation: StackAllocation = .{ .is_floating_point = variable.type.isFloat() };

                        if (variable.type.getFunction() != null) {
                            try pointerToData(text_section_writer, "8", name);

                            try self.pushRegister(text_section_writer, "8", stack_allocation);
                        } else {
                            try copyFromData(text_section_writer, "8", stack_allocation, name);

                            try self.pushRegister(text_section_writer, "8", stack_allocation);
                        }
                    },
                }
            },

            .get_ptr => |name| {
                const variable = self.variables.get(name).?;

                switch (variable.linkage) {
                    .local => {
                        try pointerToStack(text_section_writer, "8", self.stack_offsets.items[variable.stack_index + 1]);

                        try self.pushRegister(text_section_writer, "8", .{ .is_floating_point = false });
                    },

                    .global => {
                        try pointerToData(text_section_writer, "8", name);

                        try self.pushRegister(text_section_writer, "8", .{ .is_floating_point = false });
                    },
                }
            },

            .string => |string| {
                if (self.stack_offsets.items.len == 0) {
                    try rodata_section_writer.print("\t.asciz \"{s}\"\n", .{string});
                } else {
                    try rodata_section_writer.print("\tstr{}: .asciz \"{s}\"\n", .{ self.string_literals_index, string });
                    try text_section_writer.print("\tleaq str{}(%rip), %r8\n", .{self.string_literals_index});

                    self.string_literals_index += 1;

                    try self.pushRegister(text_section_writer, "8", .{ .is_floating_point = false });
                }
            },

            .int => |int| {
                if (self.stack_offsets.items.len == 0) {
                    try data_section_writer.print("\t.quad {}\n", .{int});
                } else {
                    try text_section_writer.print("\tmovq ${}, %r8\n", .{int});

                    try self.pushRegister(text_section_writer, "8", .{ .is_floating_point = false });
                }
            },

            .float => |float| {
                if (self.stack_offsets.items.len == 0) {
                    try data_section_writer.print("\t.quad {}\n", .{@as(u64, @bitCast(float))});
                } else {
                    try rodata_section_writer.print("flt{}: .quad {}\n", .{ self.floating_points_index, @as(u64, @bitCast(float)) });
                    try text_section_writer.print("\tmovsd flt{}(%rip), %xmm8\n", .{self.floating_points_index});

                    self.floating_points_index += 1;

                    try self.pushRegister(text_section_writer, "8", .{ .is_floating_point = true });
                }
            },

            .negate => {
                const stack_allocation = self.stack.getLast();

                try self.popRegister(text_section_writer, "bx");

                if (stack_allocation.is_floating_point) {
                    try text_section_writer.writeAll("\tmovq %xmm1, %rbx\n");
                    try text_section_writer.writeAll("\tmovabsq $0x8000000000000000, %rax\n");
                    try text_section_writer.writeAll("\txorq %rax, %rbx\n");
                    try text_section_writer.writeAll("\tmovsd %rbx, %xmm1\n");
                } else {
                    try text_section_writer.writeAll("\txorq %rax, %rax\n");
                    try text_section_writer.writeAll("\tsubq %rax, %rbx\n");
                }

                try self.pushRegister(text_section_writer, "bx", stack_allocation);
            },

            .add, .sub, .mul, .div => {
                const stack_allocation = self.stack.getLast();

                try self.popRegister(text_section_writer, "cx");
                try self.popRegister(text_section_writer, "ax");

                const binary_operation_str = switch (instruction) {
                    .add => "add",
                    .sub => "sub",
                    .mul => "mul",
                    .div => "div",

                    else => unreachable,
                };

                if (stack_allocation.is_floating_point) {
                    try text_section_writer.print("\t{s}sd %xmm1, %xmm0\n", .{binary_operation_str});
                } else {
                    if (instruction == .mul) {
                        try text_section_writer.print("\t{s}q %rcx\n", .{binary_operation_str});
                    } else if (instruction == .div) {
                        try text_section_writer.writeAll("\tcqto\n");

                        try text_section_writer.print("\t{s}q %rcx\n", .{binary_operation_str});
                    } else {
                        try text_section_writer.print("\t{s}q %rcx, %rax\n", .{binary_operation_str});
                    }
                }

                try self.pushRegister(text_section_writer, "ax", stack_allocation);
            },

            .pop => {
                try self.popRegister(text_section_writer, "8");
            },

            .assembly => |content| {
                try text_section_writer.print("{s}\n", .{content});
            },

            .@"return" => {
                try text_section_writer.writeAll("\tretq\n");
            },
        }
    }
}

fn suffixToNumber(register_suffix: []const u8) u8 {
    // We only use the ax suffix for now, update this later
    if (std.mem.eql(u8, register_suffix, "ax")) {
        return 0;
    } else if (std.mem.eql(u8, register_suffix, "bx")) {
        return 1;
    } else if (std.mem.eql(u8, register_suffix, "cx")) {
        return 2;
    } else if (std.mem.eql(u8, register_suffix, "dx")) {
        return 3;
    } else if (std.mem.eql(u8, register_suffix, "si")) {
        return 4;
    } else if (std.mem.eql(u8, register_suffix, "di")) {
        return 5;
    } else {
        return register_suffix[0] - '0';
    }
}

fn pushRegister(
    self: *x86_64,
    text_section_writer: anytype,
    register_suffix: []const u8,
    stack_allocation: StackAllocation,
) Error!void {
    try self.stack.append(self.allocator, stack_allocation);

    const stack_offset = self.stack_offsets.getLast() + self.stack_alignment;

    try self.stack_offsets.append(self.allocator, stack_offset);

    try copyToStack(text_section_writer, register_suffix, stack_allocation, stack_offset);
}

fn popRegister(self: *x86_64, text_section_writer: anytype, register_suffix: []const u8) Error!void {
    const stack_allocation = self.stack.pop();

    const stack_offset = self.stack_offsets.pop();

    try copyFromStack(text_section_writer, register_suffix, stack_allocation, stack_offset);
}

fn pointerToStack(
    text_section_writer: anytype,
    register_suffix: []const u8,
    stack_offset: usize,
) Error!void {
    try text_section_writer.print("\tleaq -{}(%rbp), %r{s}\n", .{ stack_offset, register_suffix });
}

fn pointerToData(
    text_section_writer: anytype,
    register_suffix: []const u8,
    data_name: []const u8,
) Error!void {
    try text_section_writer.print("\tleaq {s}(%rip), %r{s}\n", .{ data_name, register_suffix });
}

fn copyToStack(
    text_section_writer: anytype,
    register_suffix: []const u8,
    stack_allocation: StackAllocation,
    stack_offset: usize,
) Error!void {
    if (stack_allocation.is_floating_point) {
        try text_section_writer.print("\tmovsd %xmm{}, -{}(%rbp)\n", .{ suffixToNumber(register_suffix), stack_offset });
    } else {
        try text_section_writer.print("\tmovq %r{s}, -{}(%rbp)\n", .{ register_suffix, stack_offset });
    }
}

fn copyFromStack(
    text_section_writer: anytype,
    register_suffix: []const u8,
    stack_allocation: StackAllocation,
    stack_offset: usize,
) Error!void {
    if (stack_allocation.is_floating_point) {
        try text_section_writer.print("\tmovsd -{}(%rbp), %xmm{}\n", .{ stack_offset, suffixToNumber(register_suffix) });
    } else {
        try text_section_writer.print("\tmovq -{}(%rbp), %r{s}\n", .{ stack_offset, register_suffix });
    }
}

fn copyToData(
    text_section_writer: anytype,
    register_suffix: []const u8,
    stack_allocation: StackAllocation,
    data_name: []const u8,
) Error!void {
    if (stack_allocation.is_floating_point) {
        try text_section_writer.print("\tmovq %xmm{}, {s}(%rip)\n", .{ suffixToNumber(register_suffix), data_name });
    } else {
        try text_section_writer.print("\tmovq %r{s}, {s}(%rip)\n", .{ register_suffix, data_name });
    }
}

fn copyFromData(
    text_section_writer: anytype,
    register_suffix: []const u8,
    stack_allocation: StackAllocation,
    data_name: []const u8,
) Error!void {
    if (stack_allocation.is_floating_point) {
        try text_section_writer.print("\tmovq {s}(%rip), %xmm{}\n", .{ data_name, suffixToNumber(register_suffix) });
    } else {
        try text_section_writer.print("\tmovq {s}(%rip), %r{s}\n", .{ data_name, register_suffix });
    }
}

pub fn dump(self: *x86_64) Error![]const u8 {
    var result: std.ArrayListUnmanaged(u8) = .{};

    const result_writer = result.writer(self.allocator);

    if (self.assembly.text_section.items.len > 0) {
        try result_writer.writeAll(".section \".text\"\n");
        try result_writer.writeAll(self.assembly.text_section.items);
    }

    if (self.assembly.data_section.items.len > 0) {
        try result_writer.writeAll(".section \".data\"\n");
        try result_writer.writeAll(self.assembly.data_section.items);
    }

    if (self.assembly.rodata_section.items.len > 0) {
        try result_writer.writeAll(".section \".rodata\"\n");
        try result_writer.writeAll(self.assembly.rodata_section.items);
    }

    return result.toOwnedSlice(self.allocator);
}
