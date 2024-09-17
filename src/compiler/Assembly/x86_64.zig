const std = @import("std");

const Assembly = @import("../Assembly.zig");
const Lir = @import("../Lir.zig");
const Symbol = @import("../Symbol.zig");
const Type = @import("../Type.zig");

const x86_64 = @This();

allocator: std.mem.Allocator,

assembly: Assembly = .{},

lir: Lir,

stack: std.ArrayListUnmanaged(StackAllocation) = .{},
stack_offsets: std.ArrayListUnmanaged(usize) = .{},
stack_alignment: usize = 16,

scope_stack: std.ArrayListUnmanaged(Symbol.Scope(Variable)) = .{},
scope: *Symbol.Scope(Variable) = undefined,

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

    var lir_data_block_iterator = self.lir.data_blocks.iterator();

    while (lir_data_block_iterator.next()) |lir_block_entry| {
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

            if (lir_block.is_control_flow) {
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
        .parameter => |parameter| try self.renderParameter(text_section_writer, parameter),

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

        .bool_not, .bit_not => try self.renderNot(text_section_writer, lir_instruction),

        .read => |result_type| try self.renderRead(text_section_writer, result_type),
        .write => try self.renderWrite(text_section_writer),

        .add, .sub, .mul, .div, .bit_and, .bit_or, .bit_xor, .shl, .shr => try self.renderBinaryOperation(text_section_writer, lir_instruction),
        .lt, .gt, .eql => try self.renderComparisonOperation(text_section_writer, lir_instruction),

        .jmp_if_false => |block_name| try self.renderJmpIfFalse(text_section_writer, block_name),
        .jmp => |block_name| try text_section_writer.print("\tjmp .L{s}\n", .{block_name}),

        .assembly => |content| try text_section_writer.print("{s}\n", .{content}),
        .assembly_input => |register| try self.renderAssemblyInput(text_section_writer, register),
        .assembly_output => |register| try self.renderAssemblyOutput(text_section_writer, register),

        .pop => try self.popRegister(text_section_writer, "8"),

        .@"return" => try self.renderReturn(text_section_writer, lir_block.is_control_flow),
    }
}

fn renderParameter(self: *x86_64, text_section_writer: anytype, parameter: struct { usize, Symbol }) Error!void {
    const function_parameter_index, const function_parameter_symbol = parameter;

    const is_floating_point = function_parameter_symbol.type.isFloat();

    if (is_floating_point) {
        try text_section_writer.print("\tmovq {}(%rbp), %xmm8\n", .{(function_parameter_index + 1) * self.stack_alignment});
    } else {
        try text_section_writer.print("\tmovq {}(%rbp), %r8\n", .{(function_parameter_index + 1) * self.stack_alignment});
    }

    try self.pushRegister(text_section_writer, "8", .{ .is_floating_point = is_floating_point });

    try self.scope.put(
        self.allocator,
        function_parameter_symbol.name.buffer,
        .{
            .maybe_stack_index = self.stack.items.len - 1,
            .type = function_parameter_symbol.type,
            .linkage = function_parameter_symbol.linkage,
        },
    );
}

fn renderCall(self: *x86_64, text_section_writer: anytype, function: Type.Data.Function) Error!void {
    try self.popRegister(text_section_writer, "8");

    var i: usize = function.parameter_types.len;

    const stack_top_offset = (i + self.stack.items.len) * self.stack_alignment;

    try text_section_writer.print("\tsubq ${}, %rsp\n", .{stack_top_offset});

    while (i > 0) {
        i -= 1;

        const parameter_type = function.parameter_types[i];

        try self.popRegister(text_section_writer, "9");

        if (parameter_type.isFloat()) {
            try text_section_writer.print("\tmovq %xmm9, {}(%rsp)\n", .{i * self.stack_alignment});
        } else {
            try text_section_writer.print("\tmovq %r9, {}(%rsp)\n", .{i * self.stack_alignment});
        }
    }

    try text_section_writer.writeAll("\tcallq *%r8\n");

    try text_section_writer.print("\taddq ${}, %rsp\n", .{stack_top_offset});

    try self.pushRegister(text_section_writer, "ax", .{ .is_floating_point = function.return_type.isFloat() });
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

        try self.popRegister(text_section_writer, "8");

        try copyToData(text_section_writer, "8", stack_allocation, name);
    } else if (variable.maybe_stack_index) |stack_index| {
        const stack_allocation = self.stack.items[stack_index];

        try self.popRegister(text_section_writer, "8");

        try copyToStack(text_section_writer, "8", stack_allocation, self.stack_offsets.items[stack_index + 1]);
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

            try copyFromStack(text_section_writer, "8", stack_allocation, self.stack_offsets.items[stack_index + 1]);

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
}

fn renderGetPtr(self: *x86_64, text_section_writer: anytype, name: []const u8) Error!void {
    const variable = self.scope.get(name).?;

    switch (variable.linkage) {
        .local => {
            const stack_index = variable.maybe_stack_index.?;

            try pointerToStack(text_section_writer, "8", self.stack_offsets.items[stack_index + 1]);

            try self.pushRegister(text_section_writer, "8", .{ .is_floating_point = false });
        },

        .global => {
            try pointerToData(text_section_writer, "8", name);

            try self.pushRegister(text_section_writer, "8", .{ .is_floating_point = false });
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

        try self.pushRegister(text_section_writer, "8", .{ .is_floating_point = false });
    }
}

fn renderInt(self: *x86_64, text_section_writer: anytype, data_section_writer: anytype, value: i128) Error!void {
    if (self.stack_offsets.items.len == 0) {
        try data_section_writer.print("\t.quad {}\n", .{value});
    } else {
        try text_section_writer.print("\tmovq ${}, %r8\n", .{value});

        try self.pushRegister(text_section_writer, "8", .{ .is_floating_point = false });
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

        try self.pushRegister(text_section_writer, "8", .{ .is_floating_point = true });
    }
}

fn renderBoolean(self: *x86_64, text_section_writer: anytype, data_section_writer: anytype, value: bool) Error!void {
    if (self.stack_offsets.items.len == 0) {
        try data_section_writer.print("\t.quad {}\n", .{@intFromBool(value)});
    } else {
        try text_section_writer.print("\tmovq ${}, %r8\n", .{@intFromBool(value)});

        try self.pushRegister(text_section_writer, "8", .{ .is_floating_point = false });
    }
}

fn renderNegate(self: *x86_64, text_section_writer: anytype) Error!void {
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
}

fn renderNot(self: *x86_64, text_section_writer: anytype, instruction: Lir.Block.Instruction) Error!void {
    try self.popRegister(text_section_writer, "ax");
    if (instruction == .bool_not) {
        try text_section_writer.writeAll("\txorq $1, %rax\n");
    } else {
        try text_section_writer.writeAll("\txorq $-1, %rax\n");
    }

    try self.pushRegister(text_section_writer, "ax", .{ .is_floating_point = false });
}

fn renderRead(self: *x86_64, text_section_writer: anytype, result_type: Type) Error!void {
    const stack_allocation: StackAllocation = .{ .is_floating_point = result_type.isFloat() };

    try self.popRegister(text_section_writer, "ax");

    if (stack_allocation.is_floating_point) {
        try text_section_writer.writeAll("\tmovq 0(%rax), %xmm1\n");
    } else {
        try text_section_writer.writeAll("\tmovq 0(%rax), %rbx\n");
    }

    try self.pushRegister(text_section_writer, "bx", stack_allocation);
}

fn renderWrite(self: *x86_64, text_section_writer: anytype) Error!void {
    const stack_allocation = self.stack.pop();

    try self.popRegister(text_section_writer, "ax");
    try self.popRegister(text_section_writer, "bx");

    if (stack_allocation.is_floating_point) {
        try text_section_writer.writeAll("\tmovq %xmm0, 0(%rbx)\n");
    } else {
        try text_section_writer.writeAll("\tmovq %rax, 0(%rbx)\n");
    }
}

fn renderBinaryOperation(self: *x86_64, text_section_writer: anytype, instruction: Lir.Block.Instruction) Error!void {
    const stack_allocation = self.stack.getLast();

    try self.popRegister(text_section_writer, "cx");
    try self.popRegister(text_section_writer, "ax");

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
        try text_section_writer.print("\t{s}sd %xmm2, %xmm0\n", .{binary_operation_str});
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

    try self.pushRegister(text_section_writer, "ax", stack_allocation);
}

fn renderComparisonOperation(self: *x86_64, text_section_writer: anytype, instruction: Lir.Block.Instruction) Error!void {
    const stack_allocation = self.stack.getLast();

    try self.popRegister(text_section_writer, "cx");
    try self.popRegister(text_section_writer, "ax");

    const set_str = switch (instruction) {
        .lt => "setl",
        .gt => "setg",
        .eql => "sete",

        else => unreachable,
    };

    if (stack_allocation.is_floating_point) {
        try text_section_writer.writeAll("\tucomisd %xmm2, %xmm0\n");
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

    try self.pushRegister(text_section_writer, "ax", .{ .is_floating_point = false });
}

fn renderJmpIfFalse(self: *x86_64, text_section_writer: anytype, block_name: []const u8) Error!void {
    try self.popRegister(text_section_writer, "8");

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
        try self.popRegister(text_section_writer, "ax");
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

fn suffixToNumber(register_suffix: []const u8) u8 {
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
