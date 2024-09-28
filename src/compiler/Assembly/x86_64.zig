const std = @import("std");

const Assembly = @import("../Assembly.zig");
const Lir = @import("../Lir.zig");
const Symbol = @import("../Symbol.zig");
const Scope = Symbol.Scope;
const Type = Symbol.Type;

const x86_64 = @This();

allocator: std.mem.Allocator,

assembly: Assembly = .{},

lir: Lir,

stack: std.ArrayListUnmanaged(StackAllocation) = .{},
stack_offsets: std.ArrayListUnmanaged(usize) = .{},
stack_alignment: usize = 8,

scope_stack: std.ArrayListUnmanaged(Scope(Variable)) = .{},
scope: *Scope(Variable) = undefined,

string_literals_index: usize = 0,
floating_points_index: usize = 0,

pub const Variable = struct {
    maybe_stack_index: ?usize = null,
    type: Type,
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
    self.stack.clearAndFree(self.allocator);
    self.stack_offsets.clearAndFree(self.allocator);
}

pub const Error = std.mem.Allocator.Error;

pub fn render(self: *x86_64) Error!void {
    const text_section_writer = self.assembly.text_section.writer(self.allocator);
    const data_section_writer = self.assembly.data_section.writer(self.allocator);
    const rodata_section_writer = self.assembly.rodata_section.writer(self.allocator);

    const global_scope = try self.scope_stack.addOne(self.allocator);
    global_scope.* = .{};

    self.scope = global_scope;

    try self.renderFunctionTypes();
    try self.analyzeExternalTypes();
    try self.renderGlobalBlocks(text_section_writer, data_section_writer, rodata_section_writer);
    try self.renderFunctionBlocks(text_section_writer, data_section_writer, rodata_section_writer);
}

fn renderGlobalBlocks(
    self: *x86_64,
    text_section_writer: anytype,
    data_section_writer: anytype,
    rodata_section_writer: anytype,
) Error!void {
    var lir_block_iterator = self.lir.global.iterator();

    while (lir_block_iterator.next()) |lir_block_entry| {
        const lir_block_name = lir_block_entry.key_ptr.*;
        const lir_block = lir_block_entry.value_ptr;

        try data_section_writer.print(".global {s}\n", .{lir_block_name});
        try data_section_writer.print("{s}:\n", .{lir_block_name});

        for (lir_block.instructions.items) |lir_instruction| {
            try self.renderInstruction(
                text_section_writer,
                data_section_writer,
                rodata_section_writer,
                lir_block,
                lir_instruction,
            );
        }
    }
}

fn analyzeExternalTypes(self: *x86_64) Error!void {
    var lir_type_iterator = self.lir.external.iterator();

    while (lir_type_iterator.next()) |lir_type_entry| {
        const lir_type_name = lir_type_entry.key_ptr.*;
        const lir_type = lir_type_entry.value_ptr.*;

        try self.scope.put(self.allocator, lir_type_name, .{
            .type = lir_type,
            .linkage = .external,
        });
    }
}

fn renderFunctionTypes(self: *x86_64) Error!void {
    var lir_function_iterator = self.lir.functions.iterator();

    while (lir_function_iterator.next()) |lir_function_entry| {
        const lir_function = lir_function_entry.value_ptr.*;

        try self.scope.put(
            self.allocator,
            lir_function.name,
            .{
                .type = lir_function.type,
                .linkage = .global,
            },
        );
    }
}

fn renderFunctionBlocks(
    self: *x86_64,
    text_section_writer: anytype,
    data_section_writer: anytype,
    rodata_section_writer: anytype,
) Error!void {
    var lir_function_iterator = self.lir.functions.iterator();

    while (lir_function_iterator.next()) |lir_function_entry| {
        const lir_function = lir_function_entry.value_ptr.*;

        try text_section_writer.print(".global {s}\n", .{lir_function.name});
        try text_section_writer.print("{s}:\n", .{lir_function.name});

        try text_section_writer.writeAll("\tpushq %rbp\n");
        try text_section_writer.writeAll("\tmovq %rsp, %rbp\n");

        try self.stack_offsets.append(self.allocator, 0);

        var lir_block_iterator = lir_function.blocks.iterator();

        while (lir_block_iterator.next()) |lir_block_entry| {
            const lir_block_name = lir_block_entry.key_ptr.*;
            const lir_block = lir_block_entry.value_ptr;

            if (!std.mem.eql(u8, lir_block_name, "entry")) {
                try text_section_writer.print(".L{s}:\n", .{lir_block_name});
            }

            const previous_stack_len = self.stack.items.len;

            const local_scope = try self.scope_stack.addOne(self.allocator);
            local_scope.* = .{ .maybe_parent = self.scope };
            self.scope = local_scope;

            for (lir_block.instructions.items) |lir_instruction| {
                try self.renderInstruction(
                    text_section_writer,
                    data_section_writer,
                    rodata_section_writer,
                    lir_block,
                    lir_instruction,
                );
            }

            if (lir_block.tag == .control_flow) {
                self.stack.shrinkRetainingCapacity(previous_stack_len);
            }
        }
    }
}

fn renderInstruction(
    self: *x86_64,
    text_section_writer: anytype,
    data_section_writer: anytype,
    rodata_section_writer: anytype,
    lir_block: *Lir.Block,
    lir_instruction: Lir.Block.Instruction,
) Error!void {
    switch (lir_instruction) {
        .parameters => |parameters| try self.renderParameters(text_section_writer, parameters),

        .call => |function| try self.renderCall(text_section_writer, function),

        .variable => |symbol| try self.renderVariable(symbol),

        .set => |name| try self.renderSet(text_section_writer, name),
        .get => |name| try self.renderGet(text_section_writer, name),
        .get_ptr => |name| try self.renderGetPtr(text_section_writer, name),

        .string => |content| try self.renderString(text_section_writer, rodata_section_writer, content),
        .int => |value| try self.renderInt(text_section_writer, data_section_writer, value),
        .float => |value| try self.renderFloat(text_section_writer, data_section_writer, rodata_section_writer, value),
        .boolean => |value| try self.renderBoolean(text_section_writer, data_section_writer, value),

        .negate => try self.renderNegate(text_section_writer),

        .bool_not, .bit_not => try self.renderNot(text_section_writer),

        .read => |result_type| try self.renderRead(text_section_writer, result_type),
        .write => try self.renderWrite(text_section_writer),

        .add, .sub, .mul, .div, .bit_and, .bit_or, .bit_xor, .shl, .shr => try self.renderBinaryOperation(text_section_writer, lir_instruction),
        .lt, .gt, .eql => try self.renderComparisonOperation(text_section_writer, lir_instruction),

        .jmp_if_false => |block_name| try self.renderJmpIfFalse(text_section_writer, block_name),
        .jmp => |block_name| try text_section_writer.print("\tjmp .L{s}\n", .{block_name}),

        .assembly => |content| try text_section_writer.print("{s}\n", .{content}),
        .assembly_input => |register| try self.renderAssemblyInput(text_section_writer, register),
        .assembly_output => |register| try self.renderAssemblyOutput(text_section_writer, register),

        .pop => try self.popRegister(text_section_writer, "r8", "xmm8"),

        .@"return" => try self.renderReturn(text_section_writer, lir_block.tag == .control_flow),
    }
}

const system_v_int_registers: [6][]const u8 = .{ "rdi", "rsi", "rdx", "rcx", "r8", "r9" };
const system_v_float_registers: [8][]const u8 = .{ "xmm0", "xmm1", "xmm2", "xmm3", "xmm4", "xmm5", "xmm6", "xmm7" };

fn renderParameters(self: *x86_64, text_section_writer: anytype, parameters: []const Symbol) Error!void {
    var next_int_register_index: usize = 0;
    var next_float_register_index: usize = 0;

    for (parameters, 0..) |parameter, i| {
        const is_floating_point = parameter.type.isFloat();

        if (is_floating_point and next_float_register_index < system_v_float_registers.len) {
            const float_register = system_v_float_registers[next_float_register_index];
            next_float_register_index += 1;

            try self.pushRegister(text_section_writer, undefined, float_register, .{ .is_floating_point = true });
        } else if (!is_floating_point and next_int_register_index < system_v_int_registers.len) {
            const integer_register = system_v_int_registers[next_int_register_index];
            next_int_register_index += 1;

            try self.pushRegister(text_section_writer, integer_register, undefined, .{ .is_floating_point = false });
        } else {
            const j = i - (next_int_register_index + next_float_register_index) + 2;

            if (is_floating_point) {
                try text_section_writer.print("\tmovq {}(%rbp), %xmm8\n", .{j * self.stack_alignment});
            } else {
                try text_section_writer.print("\tmovq {}(%rbp), %r8\n", .{j * self.stack_alignment});
            }

            try self.pushRegister(text_section_writer, "r8", "xmm8", .{ .is_floating_point = is_floating_point });
        }

        try self.scope.put(
            self.allocator,
            parameter.name.buffer,
            .{
                .maybe_stack_index = self.stack.items.len - 1,
                .type = parameter.type,
                .linkage = parameter.linkage,
            },
        );
    }
}

fn renderCall(self: *x86_64, text_section_writer: anytype, function: Type.Function) Error!void {
    try self.popRegister(text_section_writer, "r10", undefined);

    // A rough estimate of the stack space needed to call this function, it may be less because of storing parameters in registers
    const stack_top_offset = (function.parameter_types.len + self.stack.items.len) * self.stack_alignment;

    try text_section_writer.print("\tsubq ${}, %rsp\n", .{stack_top_offset});

    var next_int_register_index: usize = 0;
    var next_float_register_index: usize = 0;

    for (function.parameter_types, 0..) |parameter_type, i| {
        const is_floating_point = parameter_type.isFloat();

        if (is_floating_point and next_float_register_index < system_v_float_registers.len) {
            const float_register = system_v_float_registers[next_float_register_index];
            next_float_register_index += 1;

            try self.popRegister(text_section_writer, undefined, float_register);
        } else if (!is_floating_point and next_int_register_index < system_v_int_registers.len) {
            const integer_register = system_v_int_registers[next_int_register_index];
            next_int_register_index += 1;

            try self.popRegister(text_section_writer, integer_register, undefined);
        } else {
            const j = i - (next_int_register_index + next_float_register_index);

            try self.popRegister(text_section_writer, "r11", "xmm8");

            if (is_floating_point) {
                try text_section_writer.print("\tmovq %xmm8, {}(%rsp)\n", .{j * self.stack_alignment});
            } else {
                try text_section_writer.print("\tmovq %r11, {}(%rsp)\n", .{j * self.stack_alignment});
            }
        }
    }

    try text_section_writer.writeAll("\tcallq *%r10\n");

    try text_section_writer.print("\taddq ${}, %rsp\n", .{stack_top_offset});

    try self.pushRegister(text_section_writer, "rax", "xmm0", .{ .is_floating_point = function.return_type.isFloat() });
}

fn renderVariable(self: *x86_64, symbol: Symbol) Error!void {
    try self.scope.put(
        self.allocator,
        symbol.name.buffer,
        .{
            .type = symbol.type,
            .linkage = symbol.linkage,
        },
    );
}

fn renderSet(self: *x86_64, text_section_writer: anytype, name: []const u8) Error!void {
    const variable = self.scope.getPtr(name).?;

    if (variable.linkage == .global) {
        const stack_allocation = self.stack.pop();

        try self.popRegister(text_section_writer, "r8", "xmm8");

        try copyToData(text_section_writer, "r8", "xmm8", stack_allocation, name);
    } else if (variable.maybe_stack_index) |stack_index| {
        const stack_allocation = self.stack.items[stack_index];

        try self.popRegister(text_section_writer, "r8", "xmm8");

        try copyToStack(
            text_section_writer,
            "r8",
            "xmm8",
            stack_allocation,
            self.stack_offsets.items[stack_index + 1],
        );
    } else {
        variable.maybe_stack_index = self.stack.items.len - 1;
    }
}

fn renderGet(self: *x86_64, text_section_writer: anytype, name: []const u8) Error!void {
    const variable = self.scope.get(name).?;

    switch (variable.linkage) {
        .local => {
            const stack_index = variable.maybe_stack_index.?;

            const stack_allocation = self.stack.items[stack_index];

            try copyFromStack(
                text_section_writer,
                "r8",
                "xmm8",
                stack_allocation,
                self.stack_offsets.items[stack_index + 1],
            );

            try self.pushRegister(text_section_writer, "r8", "xmm8", stack_allocation);
        },

        .global, .external => {
            const stack_allocation: StackAllocation = .{ .is_floating_point = variable.type.isFloat() };

            if (variable.type.getFunction() != null) {
                if (variable.linkage == .external) {
                    try pointerToDataPlt(text_section_writer, "r8", name);
                } else {
                    try pointerToData(text_section_writer, "r8", name);
                }

                try self.pushRegister(text_section_writer, "r8", undefined, stack_allocation);
            } else {
                try copyFromData(text_section_writer, "r8", "xmm8", stack_allocation, name);

                try self.pushRegister(text_section_writer, "r8", "xmm8", stack_allocation);
            }
        },
    }
}

fn renderGetPtr(self: *x86_64, text_section_writer: anytype, name: []const u8) Error!void {
    const variable = self.scope.get(name).?;

    switch (variable.linkage) {
        .local => {
            const stack_index = variable.maybe_stack_index.?;

            try pointerToStack(text_section_writer, "r8", self.stack_offsets.items[stack_index + 1]);

            try self.pushRegister(text_section_writer, "r8", undefined, .{ .is_floating_point = false });
        },

        .global, .external => {
            if (variable.linkage == .external and variable.type.getFunction() != null) {
                try pointerToDataPlt(text_section_writer, "r8", name);
            } else {
                try pointerToData(text_section_writer, "r8", name);
            }

            try self.pushRegister(text_section_writer, "r8", undefined, .{ .is_floating_point = false });
        },
    }
}

fn renderString(self: *x86_64, text_section_writer: anytype, rodata_section_writer: anytype, content: []const u8) Error!void {
    if (self.stack_offsets.items.len == 0) {
        try rodata_section_writer.print("\t.asciz \"{s}\"\n", .{content});
    } else {
        try rodata_section_writer.print("\tstr{}: .asciz \"{s}\"\n", .{ self.string_literals_index, content });
        try text_section_writer.print("\tleaq str{}(%rip), %r8\n", .{self.string_literals_index});

        self.string_literals_index += 1;

        try self.pushRegister(text_section_writer, "r8", undefined, .{ .is_floating_point = false });
    }
}

fn renderInt(self: *x86_64, text_section_writer: anytype, data_section_writer: anytype, value: i128) Error!void {
    if (self.stack_offsets.items.len == 0) {
        try data_section_writer.print("\t.quad {}\n", .{value});
    } else {
        try text_section_writer.print("\tmovq ${}, %r8\n", .{value});

        try self.pushRegister(text_section_writer, "r8", undefined, .{ .is_floating_point = false });
    }
}

fn renderFloat(
    self: *x86_64,
    text_section_writer: anytype,
    data_section_writer: anytype,
    rodata_section_writer: anytype,
    value: f64,
) Error!void {
    if (self.stack_offsets.items.len == 0) {
        try data_section_writer.print("\t.quad {}\n", .{@as(u64, @bitCast(value))});
    } else {
        try rodata_section_writer.print("flt{}: .quad {}\n", .{ self.floating_points_index, @as(u64, @bitCast(value)) });
        try text_section_writer.print("\tmovsd flt{}(%rip), %xmm8\n", .{self.floating_points_index});

        self.floating_points_index += 1;

        try self.pushRegister(text_section_writer, undefined, "xmm8", .{ .is_floating_point = true });
    }
}

fn renderBoolean(self: *x86_64, text_section_writer: anytype, data_section_writer: anytype, value: bool) Error!void {
    if (self.stack_offsets.items.len == 0) {
        try data_section_writer.print("\t.quad {}\n", .{@intFromBool(value)});
    } else {
        try text_section_writer.print("\tmovq ${}, %r8\n", .{@intFromBool(value)});

        try self.pushRegister(text_section_writer, "r8", undefined, .{ .is_floating_point = false });
    }
}

fn renderNegate(self: *x86_64, text_section_writer: anytype) Error!void {
    const stack_allocation = self.stack.getLast();

    try self.popRegister(text_section_writer, "rcx", "xmm0");

    if (stack_allocation.is_floating_point) {
        try text_section_writer.writeAll("\tmovq %xmm0, %rcx\n");
        try text_section_writer.writeAll("\tmovabsq $0x8000000000000000, %rax\n");
        try text_section_writer.writeAll("\txorq %rax, %rcx\n");
        try text_section_writer.writeAll("\tmovsd %rcx, %xmm0\n");
    } else {
        try text_section_writer.writeAll("\txorq %rax, %rax\n");
        try text_section_writer.writeAll("\tsubq %rax, %rcx\n");
    }

    try self.pushRegister(text_section_writer, "rcx", "xmm0", stack_allocation);
}

fn renderNot(self: *x86_64, text_section_writer: anytype) Error!void {
    try self.popRegister(text_section_writer, "rax", undefined);

    try text_section_writer.writeAll("\txorq $-1, %rax\n");

    try self.pushRegister(text_section_writer, "rax", undefined, .{ .is_floating_point = false });
}

fn renderRead(self: *x86_64, text_section_writer: anytype, result_type: Type) Error!void {
    const stack_allocation: StackAllocation = .{ .is_floating_point = result_type.isFloat() };

    try self.popRegister(text_section_writer, "rax", undefined);

    if (stack_allocation.is_floating_point) {
        try text_section_writer.writeAll("\tmovq 0(%rax), %xmm0\n");
    } else {
        try text_section_writer.writeAll("\tmovq 0(%rax), %rcx\n");
    }

    try self.pushRegister(text_section_writer, "rcx", "xmm0", stack_allocation);
}

fn renderWrite(self: *x86_64, text_section_writer: anytype) Error!void {
    const stack_allocation = self.stack.pop();

    try self.popRegister(text_section_writer, "rcx", "xmm0");
    try self.popRegister(text_section_writer, "rax", undefined);

    if (stack_allocation.is_floating_point) {
        try text_section_writer.writeAll("\tmovq %xmm0, 0(%rax)\n");
    } else {
        try text_section_writer.writeAll("\tmovq %rcx, 0(%rax)\n");
    }
}

fn renderBinaryOperation(self: *x86_64, text_section_writer: anytype, instruction: Lir.Block.Instruction) Error!void {
    const stack_allocation = self.stack.getLast();

    try self.popRegister(text_section_writer, "rcx", "xmm0");
    try self.popRegister(text_section_writer, "rax", "xmm1");

    const binary_operation_str = switch (instruction) {
        .add => "add",
        .sub => "sub",
        .mul => "mul",
        .div => "div",
        .bit_and => "and",
        .bit_or => "or",
        .bit_xor => "xor",
        .shl => "shl",
        .shr => "shr",

        else => unreachable,
    };

    if (stack_allocation.is_floating_point) {
        try text_section_writer.print("\t{s}sd %xmm0, %xmm1\n", .{binary_operation_str});
    } else {
        if (instruction == .mul) {
            try text_section_writer.print("\t{s}q %rcx\n", .{binary_operation_str});
        } else if (instruction == .div) {
            try text_section_writer.writeAll("\tcqto\n");

            try text_section_writer.print("\t{s}q %rcx\n", .{binary_operation_str});
        } else if (instruction == .shl or instruction == .shr) {
            try text_section_writer.print("\t{s}q %cl, %rax\n", .{binary_operation_str});
        } else {
            try text_section_writer.print("\t{s}q %rcx, %rax\n", .{binary_operation_str});
        }
    }

    try self.pushRegister(text_section_writer, "rax", "xmm1", stack_allocation);
}

fn renderComparisonOperation(self: *x86_64, text_section_writer: anytype, instruction: Lir.Block.Instruction) Error!void {
    const stack_allocation = self.stack.getLast();

    try self.popRegister(text_section_writer, "rcx", "xmm0");
    try self.popRegister(text_section_writer, "rax", "xmm1");

    const set_str = switch (instruction) {
        .lt => "setl",
        .gt => "setg",
        .eql => "sete",

        else => unreachable,
    };

    if (stack_allocation.is_floating_point) {
        try text_section_writer.writeAll("\tucomisd %xmm0, %xmm1\n");
        try text_section_writer.print("\t{s} %al\n", .{set_str});
        try text_section_writer.writeAll("\tsetnp %cl\n");
        try text_section_writer.writeAll("\tandb %cl, %al\n");
        try text_section_writer.writeAll("\tmovzbq %al, %rax\n");
    } else {
        try text_section_writer.writeAll("\tcmpq %rcx, %rax\n");
        try text_section_writer.print("\t{s} %al\n", .{set_str});
        try text_section_writer.writeAll("\tandb $1, %al\n");
        try text_section_writer.writeAll("\tmovzbq %al, %rax\n");
    }

    try self.pushRegister(text_section_writer, "rax", "xmm1", .{ .is_floating_point = false });
}

fn renderJmpIfFalse(self: *x86_64, text_section_writer: anytype, block_name: []const u8) Error!void {
    try self.popRegister(text_section_writer, "r8", undefined);

    try text_section_writer.writeAll("\tcmpq $0, %r8\n");
    try text_section_writer.print("\tje .L{s}\n", .{block_name});
}

fn renderAssemblyInput(self: *x86_64, text_section_writer: anytype, register: []const u8) Error!void {
    _ = self.stack.pop();

    const stack_offset = self.stack_offsets.pop();

    try text_section_writer.print("\tmovq -{}(%rbp), %{s}\n", .{ stack_offset, register });
}

fn renderAssemblyOutput(self: *x86_64, text_section_writer: anytype, register: []const u8) Error!void {
    try self.stack.append(self.allocator, .{ .is_floating_point = if (register.len > 0) register[0] == 'x' else false });

    const stack_offset = self.stack_offsets.getLast() + self.stack_alignment;

    try self.stack_offsets.append(self.allocator, stack_offset);

    try text_section_writer.print("\tmovq %{s}, -{}(%rbp)\n", .{ register, stack_offset });
}

fn renderReturn(self: *x86_64, text_section_writer: anytype, is_control_flow: bool) Error!void {
    if (self.stack.items.len != 0) {
        try self.popRegister(text_section_writer, "rax", "xmm0");
    }

    if (!is_control_flow) {
        self.stack.clearRetainingCapacity();
        self.stack_offsets.clearRetainingCapacity();
    }

    self.scope.clearAndFree(self.allocator);
    self.scope = self.scope.maybe_parent.?;
    _ = self.scope_stack.pop();

    try text_section_writer.writeAll("\tpopq %rbp\n");
    try text_section_writer.writeAll("\tretq\n");
}

fn pushRegister(
    self: *x86_64,
    text_section_writer: anytype,
    integer_register: []const u8,
    float_register: []const u8,
    stack_allocation: StackAllocation,
) Error!void {
    try self.stack.append(self.allocator, stack_allocation);

    const stack_offset = self.stack_offsets.getLast() + self.stack_alignment;

    try self.stack_offsets.append(self.allocator, stack_offset);

    try copyToStack(text_section_writer, integer_register, float_register, stack_allocation, stack_offset);
}

fn popRegister(
    self: *x86_64,
    text_section_writer: anytype,
    integer_register: []const u8,
    float_register: []const u8,
) Error!void {
    const stack_allocation = self.stack.pop();

    const stack_offset = self.stack_offsets.pop();

    try copyFromStack(text_section_writer, integer_register, float_register, stack_allocation, stack_offset);
}

fn pointerToStack(
    text_section_writer: anytype,
    register: []const u8,
    stack_offset: usize,
) Error!void {
    try text_section_writer.print("\tleaq -{}(%rbp), %{s}\n", .{ stack_offset, register });
}

fn pointerToData(
    text_section_writer: anytype,
    register: []const u8,
    data_name: []const u8,
) Error!void {
    try text_section_writer.print("\tleaq {s}(%rip), %{s}\n", .{ data_name, register });
}

fn pointerToDataPlt(
    text_section_writer: anytype,
    register: []const u8,
    data_name: []const u8,
) Error!void {
    try text_section_writer.print("\tleaq {s}@PLT(%rip), %{s}\n", .{ data_name, register });
}

fn copyToStack(
    text_section_writer: anytype,
    integer_register: []const u8,
    float_register: []const u8,
    stack_allocation: StackAllocation,
    stack_offset: usize,
) Error!void {
    if (stack_allocation.is_floating_point) {
        try text_section_writer.print("\tmovsd %{s}, -{}(%rbp)\n", .{ float_register, stack_offset });
    } else {
        try text_section_writer.print("\tmovq %{s}, -{}(%rbp)\n", .{ integer_register, stack_offset });
    }
}

fn copyFromStack(
    text_section_writer: anytype,
    integer_register: []const u8,
    float_register: []const u8,
    stack_allocation: StackAllocation,
    stack_offset: usize,
) Error!void {
    if (stack_allocation.is_floating_point) {
        try text_section_writer.print("\tmovsd -{}(%rbp), %{s}\n", .{ stack_offset, float_register });
    } else {
        try text_section_writer.print("\tmovq -{}(%rbp), %{s}\n", .{ stack_offset, integer_register });
    }
}

fn copyToData(
    text_section_writer: anytype,
    integer_register: []const u8,
    float_register: []const u8,
    stack_allocation: StackAllocation,
    data_name: []const u8,
) Error!void {
    if (stack_allocation.is_floating_point) {
        try text_section_writer.print("\tmovq %{s}, {s}(%rip)\n", .{ float_register, data_name });
    } else {
        try text_section_writer.print("\tmovq %{s}, {s}(%rip)\n", .{ integer_register, data_name });
    }
}

fn copyFromData(
    text_section_writer: anytype,
    integer_register: []const u8,
    float_register: []const u8,
    stack_allocation: StackAllocation,
    data_name: []const u8,
) Error!void {
    if (stack_allocation.is_floating_point) {
        try text_section_writer.print("\tmovq {s}(%rip), %{s}\n", .{ data_name, float_register });
    } else {
        try text_section_writer.print("\tmovq {s}(%rip), %{s}\n", .{ data_name, integer_register });
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
