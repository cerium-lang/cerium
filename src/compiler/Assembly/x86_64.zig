const std = @import("std");

const Assembly = @import("../Assembly.zig");
const Lir = @import("../Lir.zig");
const Symbol = @import("../Symbol.zig");
const Type = @import("../Type.zig");

const x86_64 = @This();

allocator: std.mem.Allocator,

assembly: Assembly = .{},

lir: Lir,

variables: std.StringHashMapUnmanaged(Variable) = .{},

stack: std.ArrayListUnmanaged(StackAllocation) = .{},
stack_offsets: std.ArrayListUnmanaged(usize) = .{},
stack_alignment: usize = 16,

string_literals_index: usize = 0,
floating_points_index: usize = 0,

pub const Variable = struct {
    stack_index: usize,
    linkage: Symbol.Linkage,
};

const StackAllocation = struct {
    is_floating_point: bool,
};

pub fn init(allocator: std.mem.Allocator, lir: Lir) x86_64 {
    return x86_64{
        .allocator = allocator,
        .lir = lir,
    };
}

pub fn deinit(self: *x86_64) void {
    self.assembly.deinit(self.allocator);
    self.variables.clearAndFree(self.allocator);
    self.stack.clearAndFree(self.allocator);
    self.stack_offsets.clearAndFree(self.allocator);
}

pub const Error = std.mem.Allocator.Error;

pub fn render(self: *x86_64) Error!void {
    const text_section_writer = self.assembly.text_section.writer(self.allocator);
    const rodata_section_writer = self.assembly.rodata_section.writer(self.allocator);

    for (self.lir.instructions.items) |instruction| {
        switch (instruction) {
            .label => |name| {
                try text_section_writer.print(".global {s}\n", .{name});
                try text_section_writer.print("{s}:\n", .{name});

                try self.variables.put(
                    self.allocator,
                    name,
                    .{
                        .stack_index = 0,
                        .linkage = .global,
                    },
                );
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

                var variable_iterator = self.variables.iterator();

                while (variable_iterator.next()) |variable_entry| {
                    if (variable_entry.value_ptr.linkage == .local) {
                        _ = self.variables.remove(variable_entry.key_ptr.*);
                    }
                }

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
                    self.allocator,
                    function_parameter_symbol.name.buffer,
                    .{
                        .stack_index = self.stack.items.len - 1,
                        .linkage = .local,
                    },
                );
            },

            .call => |function| {
                try self.popRegister(text_section_writer, "8");

                for (function.parameters, 0..) |parameter, i| {
                    try self.popRegister(text_section_writer, "9");

                    if (parameter.isFloat()) {
                        try text_section_writer.print("\tmovq %xmm9, {}(%rsp)\n", .{i * self.stack_alignment});
                    } else {
                        try text_section_writer.print("\tmovq %r9, {}(%rsp)\n", .{i * self.stack_alignment});
                    }
                }

                try self.pushRegister(text_section_writer, "8", .{ .is_floating_point = false });

                try text_section_writer.writeAll("\tcall *%r8\n");

                try self.pushRegister(text_section_writer, "ax", .{ .is_floating_point = function.return_type.isFloat() });
            },

            .set => |name| {
                if (self.variables.get(name)) |variable| {
                    const stack_allocation = self.stack.items[variable.stack_index];

                    try self.popRegister(text_section_writer, "8");

                    try copyToStack(text_section_writer, "8", stack_allocation, self.stack_offsets.items[variable.stack_index + 1]);
                } else {
                    try self.variables.put(
                        self.allocator,
                        name,
                        .{
                            .stack_index = self.stack.items.len - 1,
                            .linkage = .local,
                        },
                    );
                }
            },

            .get => |name| {
                const variable = self.variables.get(name).?;

                switch (variable.linkage) {
                    .local => {
                        const value = self.stack.items[variable.stack_index];

                        try copyFromStack(text_section_writer, "8", value, self.stack_offsets.items[variable.stack_index + 1]);

                        try self.pushRegister(text_section_writer, "8", value);
                    },

                    .global => {
                        try pointerToData(text_section_writer, "8", name);

                        try self.pushRegister(text_section_writer, "8", .{ .is_floating_point = false });
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
                try rodata_section_writer.print("\tstr{}: .asciz \"{s}\"\n", .{ self.string_literals_index, string });
                try text_section_writer.print("\tleaq str{}(%rip), %r8\n", .{self.string_literals_index});

                self.string_literals_index += 1;

                try self.pushRegister(text_section_writer, "8", .{ .is_floating_point = false });
            },

            .int => |int| {
                try text_section_writer.print("\tmovq ${}, %r8\n", .{int});

                try self.pushRegister(text_section_writer, "8", .{ .is_floating_point = false });
            },

            .float => |float| {
                try rodata_section_writer.print("flt{}: .quad {}\n", .{ self.floating_points_index, @as(u64, @bitCast(float)) });
                try text_section_writer.print("\tmovsd flt{}(%rip), %xmm8\n", .{self.floating_points_index});

                self.floating_points_index += 1;

                try self.pushRegister(text_section_writer, "8", .{ .is_floating_point = true });
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

fn pointerToStack(
    text_section_writer: anytype,
    register_suffix: []const u8,
    stack_offset: usize,
) Error!void {
    try text_section_writer.print("\tleaq -{}(%rbp), %r{}\n", .{ stack_offset, suffixToNumber(register_suffix) });
}

fn pointerToData(
    text_section_writer: anytype,
    register_suffix: []const u8,
    data_name: []const u8,
) Error!void {
    try text_section_writer.print("\tleaq {s}(%rip), %r{}\n", .{ data_name, suffixToNumber(register_suffix) });
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
