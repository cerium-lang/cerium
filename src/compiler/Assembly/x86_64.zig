const std = @import("std");

const Assembly = @import("../Assembly.zig");
const Lir = @import("../Lir.zig");
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

    for (self.lir.instructions.items, 0..) |instruction, i| {
        switch (instruction) {
            .label => |label| {
                try text_section_writer.print(".global {s}\n", .{label});
                try text_section_writer.print("{s}:\n", .{label});
            },

            .function_proluge => {
                try text_section_writer.print("\tpushq %rbp\n", .{});
                try text_section_writer.print("\tmovq %rsp, %rbp\n", .{});

                try self.stack_offsets.append(self.allocator, 0);
            },

            .function_epilogue => {
                if (self.stack.items.len != 0) {
                    try self.popRegister("ax");
                }

                try text_section_writer.print("\tpopq %rbp\n", .{});

                self.variables.clearRetainingCapacity();
                self.stack.clearRetainingCapacity();
                self.stack_offsets.clearRetainingCapacity();
            },

            .function_parameter => |function_parameter| {
                const function_parameter_index = i - 2 + 1;

                const is_floating_point = function_parameter.type.isFloat();

                if (is_floating_point) {
                    try text_section_writer.print("\tmovq {}(%rbp), %xmm8\n", .{function_parameter_index * self.stack_alignment});
                } else {
                    try text_section_writer.print("\tmovq {}(%rbp), %r8\n", .{function_parameter_index * self.stack_alignment});
                }

                try self.pushRegister("8", .{ .is_floating_point = is_floating_point });

                try self.variables.put(self.allocator, function_parameter.name.buffer, .{ .stack_index = self.stack.items.len - 1 });
            },

            .set => |name| {
                if (self.variables.get(name)) |variable| {
                    const stack_allocation = self.stack.items[variable.stack_index];

                    try self.popRegister("8");

                    try self.copyToStack("8", stack_allocation, self.stack_offsets.items[variable.stack_index + 1]);
                } else {
                    try self.variables.put(self.allocator, name, .{ .stack_index = self.stack.items.len - 1 });
                }
            },

            .get => |name| {
                const variable = self.variables.get(name).?;

                const value = self.stack.items[variable.stack_index];

                try self.copyFromStack("8", value, self.stack_offsets.items[variable.stack_index + 1]);

                try self.pushRegister("8", value);
            },

            .get_ptr => |name| {
                const variable = self.variables.get(name).?;

                try self.pointerToStack("8", self.stack_offsets.items[variable.stack_index + 1]);

                try self.pushRegister("8", .{ .is_floating_point = false });
            },

            .string => |string| {
                try rodata_section_writer.print("\tstr{}: .asciz \"{s}\"\n", .{ self.string_literals_index, string });
                try text_section_writer.print("\tleaq str{}(%rip), %r8\n", .{self.string_literals_index});

                self.string_literals_index += 1;

                try self.pushRegister("8", .{ .is_floating_point = false });
            },

            .int => |int| {
                try text_section_writer.print("\tmovq ${}, %r8\n", .{int});

                try self.pushRegister("8", .{ .is_floating_point = false });
            },

            .float => |float| {
                try rodata_section_writer.print("flt{}: .quad {}\n", .{ self.floating_points_index, @as(u64, @bitCast(float)) });
                try text_section_writer.print("\tmovsd flt{}(%rip), %xmm8\n", .{self.floating_points_index});

                self.floating_points_index += 1;

                try self.pushRegister("8", .{ .is_floating_point = true });
            },

            .negate => {
                const stack_allocation = self.stack.getLast();

                try self.popRegister("bx");

                if (stack_allocation.is_floating_point) {
                    try text_section_writer.print("\tmovq %xmm1, %rbx\n", .{});
                    try text_section_writer.print("\tmovabsq $0x8000000000000000, %rax\n", .{});
                    try text_section_writer.print("\txorq %rax, %rbx\n", .{});
                    try text_section_writer.print("\tmovsd %rbx, %xmm1\n", .{});
                } else {
                    try text_section_writer.print("\txorq %rax, %rax\n", .{});
                    try text_section_writer.print("\tsubq %rax, %rbx\n", .{});
                }

                try self.pushRegister("bx", stack_allocation);
            },

            .add, .sub, .mul, .div => {
                const stack_allocation = self.stack.getLast();

                try self.popRegister("cx");
                try self.popRegister("ax");

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
                        try text_section_writer.print("\tcqto\n", .{});

                        try text_section_writer.print("\t{s}q %rcx\n", .{binary_operation_str});
                    } else {
                        try text_section_writer.print("\t{s}q %rcx, %rax\n", .{binary_operation_str});
                    }
                }

                try self.pushRegister("ax", stack_allocation);
            },

            .pop => {
                try self.popRegister("8");
            },

            .assembly => |content| {
                try text_section_writer.print("{s}\n", .{content});
            },

            .@"return" => {
                try text_section_writer.print("\tretq\n", .{});
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

fn pushRegister(self: *x86_64, register_suffix: []const u8, stack_allocation: StackAllocation) Error!void {
    try self.stack.append(self.allocator, stack_allocation);

    const stack_offset = self.stack_offsets.getLast() + self.stack_alignment;

    try self.stack_offsets.append(self.allocator, stack_offset);

    try self.copyToStack(register_suffix, stack_allocation, stack_offset);
}

fn copyFromStack(self: *x86_64, register_suffix: []const u8, stack_allocation: StackAllocation, stack_offset: usize) Error!void {
    const text_section_writer = self.assembly.text_section.writer(self.allocator);

    if (stack_allocation.is_floating_point) {
        try text_section_writer.print("\tmovsd -{}(%rbp), %xmm{}\n", .{ stack_offset, suffixToNumber(register_suffix) });
    } else {
        try text_section_writer.print("\tmovq -{}(%rbp), %r{s}\n", .{ stack_offset, register_suffix });
    }
}

fn pointerToStack(self: *x86_64, register_suffix: []const u8, stack_offset: usize) Error!void {
    const text_section_writer = self.assembly.text_section.writer(self.allocator);

    try text_section_writer.print("\tleaq -{}(%rbp), %r{}\n", .{ stack_offset, suffixToNumber(register_suffix) });
}

fn copyToStack(self: *x86_64, register_suffix: []const u8, stack_allocation: StackAllocation, stack_offset: usize) Error!void {
    const text_section_writer = self.assembly.text_section.writer(self.allocator);

    if (stack_allocation.is_floating_point) {
        try text_section_writer.print("\tmovsd %xmm{}, -{}(%rbp)\n", .{ suffixToNumber(register_suffix), stack_offset });
    } else {
        try text_section_writer.print("\tmovq %r{s}, -{}(%rbp)\n", .{ register_suffix, stack_offset });
    }
}

fn popRegister(self: *x86_64, register_suffix: []const u8) Error!void {
    const stack_allocation = self.stack.pop();

    const stack_offset = self.stack_offsets.pop();

    try self.copyFromStack(register_suffix, stack_allocation, stack_offset);
}

pub fn dump(self: *x86_64) Error![]const u8 {
    var result: std.ArrayListUnmanaged(u8) = .{};

    const result_writer = result.writer(self.allocator);

    if (self.assembly.text_section.items.len > 0) {
        try result_writer.print(".section \".text\"\n", .{});
        try result_writer.writeAll(self.assembly.text_section.items);
    }

    if (self.assembly.data_section.items.len > 0) {
        try result_writer.print(".section \".data\"\n", .{});
        try result_writer.writeAll(self.assembly.data_section.items);
    }

    if (self.assembly.rodata_section.items.len > 0) {
        try result_writer.print(".section \".rodata\"\n", .{});
        try result_writer.writeAll(self.assembly.rodata_section.items);
    }

    return result.toOwnedSlice(self.allocator);
}
