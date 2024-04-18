const std = @import("std");

const IR = @import("../IR.zig");
const Assembly = @import("../Assembly.zig");

const Amd64Renderer = @This();

assembly: Assembly,

ir: IR,

stack: std.ArrayList(RegisterInfo),
stack_offsets: std.ArrayList(usize),
stack_map: std.StringHashMap(usize),
stack_alignment: usize = 16,

floating_points: std.ArrayList(f64),

gpa: std.mem.Allocator,

const RegisterInfo = struct {
    floating_point: bool,
};

pub fn init(gpa: std.mem.Allocator, ir: IR) Amd64Renderer {
    return Amd64Renderer{ .assembly = Assembly.init(gpa), .ir = ir, .stack = std.ArrayList(RegisterInfo).init(gpa), .stack_offsets = std.ArrayList(usize).init(gpa), .stack_map = std.StringHashMap(usize).init(gpa), .floating_points = std.ArrayList(f64).init(gpa), .gpa = gpa };
}

pub const Error = std.mem.Allocator.Error;

pub fn render(self: *Amd64Renderer) Error!void {
    const text_section_writer = self.assembly.text_section.writer();

    for (self.ir.instructions) |instruction| {
        switch (instruction) {
            .store => {
                try self.pushValue(instruction.store.value);

                try self.stack_map.put(instruction.store.name, self.stack.items.len - 1);
            },

            .load => {
                try self.pushValue(instruction.load.value);
            },

            .label => {
                try text_section_writer.print(".global {s}\n", .{instruction.label.name});
                try text_section_writer.print("{s}:\n", .{instruction.label.name});

                try self.functionProluge();
            },

            .inline_assembly => {
                try text_section_writer.print("\t{s}\n", .{instruction.inline_assembly.content});
            },

            .ret => {
                if (self.stack.items.len != 0) {
                    try self.popRegister("ax");
                }

                try self.functionEpilogue();

                try text_section_writer.print("\tretq\n", .{});
            },
        }
    }
}

fn functionProluge(self: *Amd64Renderer) Error!void {
    const text_section_writer = self.assembly.text_section.writer();

    try text_section_writer.print("\tpushq %rbp\n", .{});
    try text_section_writer.print("\tmovq %rsp, %rbp\n", .{});

    try self.stack_offsets.append(0);
}

fn functionEpilogue(self: *Amd64Renderer) Error!void {
    const text_section_writer = self.assembly.text_section.writer();

    try text_section_writer.print("\tpopq %rbp\n", .{});

    self.stack.clearAndFree();
    self.stack_offsets.clearAndFree();
    self.stack_map.clearAndFree();
}

fn suffixToNumber(register_suffix: []const u8) u8 {
    // We only use the ax suffix for now, update this later
    if (std.mem.eql(u8, register_suffix, "ax")) {
        return 0;
    } else {
        return register_suffix[0] - '0';
    }
}

fn pushRegister(self: *Amd64Renderer, register_suffix: []const u8, register_info: RegisterInfo) Error!void {
    try self.stack.append(register_info);

    const stack_offset = self.stack_offsets.items[self.stack_offsets.items.len - 1] + self.stack_alignment;

    try self.stack_offsets.append(stack_offset);

    const text_section_writer = self.assembly.text_section.writer();

    if (register_info.floating_point) {
        try text_section_writer.print("\tmovsd %xmm{}, -{}(%rbp)\n", .{ suffixToNumber(register_suffix), stack_offset });
    } else {
        try text_section_writer.print("\tmovq %r{s}, -{}(%rbp)\n", .{ register_suffix, stack_offset });
    }
}

fn copyFromStack(self: *Amd64Renderer, register_suffix: []const u8, register_info: RegisterInfo, stack_offset: usize) Error!void {
    const text_section_writer = self.assembly.text_section.writer();

    if (register_info.floating_point) {
        try text_section_writer.print("\tmovsd -{}(%rbp), %xmm{}\n", .{ stack_offset, suffixToNumber(register_suffix) });
    } else {
        try text_section_writer.print("\tmovq -{}(%rbp), %r{s}\n", .{ stack_offset, register_suffix });
    }
}

fn popRegister(self: *Amd64Renderer, register_suffix: []const u8) Error!void {
    const register_info = self.stack.pop();

    const stack_offset = self.stack_offsets.pop();

    try self.copyFromStack(register_suffix, register_info, stack_offset);
}

fn pushValue(self: *Amd64Renderer, value: IR.Value) Error!void {
    const text_section_writer = self.assembly.text_section.writer();
    const data_section_writer = self.assembly.data_section.writer();

    switch (value) {
        .variable_reference => {
            const stack_loc = self.stack_map.get(value.variable_reference.name).?;

            const register_info = self.stack.items[stack_loc];

            try self.copyFromStack("8", register_info, self.stack_offsets.items[stack_loc + 1]);

            try self.pushRegister("8", register_info);
        },

        .string_reference => {
            try data_section_writer.print("\tstr{}: .asciz \"{s}\"\n", .{ value.string_reference.index, self.ir.string_literals[value.string_reference.index] });

            try text_section_writer.print("\tleaq str{}(%rip), %r8\n", .{value.string_reference.index});

            try self.pushRegister("8", .{ .floating_point = false });
        },

        .char => {
            try text_section_writer.print("\tmovq ${}, %r8\n", .{value.char.value});

            try self.pushRegister("8", .{ .floating_point = false });
        },

        .int => {
            try text_section_writer.print("\tmovq ${}, %r8\n", .{value.int.value});

            try self.pushRegister("8", .{ .floating_point = false });
        },

        .float => {
            try self.floating_points.append(value.float.value);

            try text_section_writer.print("\tmovsd flt{}, %xmm8\n", .{self.floating_points.items.len - 1});

            try self.pushRegister("8", .{ .floating_point = true });
        },
    }
}

pub fn dump(self: *Amd64Renderer) Error![]const u8 {
    var result = std.ArrayList(u8).init(self.gpa);

    const result_writer = result.writer();

    if (self.assembly.text_section.items.len > 0) {
        try result_writer.print(".section \".text\"\n", .{});

        for (self.floating_points.items, 0..) |floating_point, i| {
            try result_writer.print("flt{}: .quad {}\n", .{ i, @as(u64, @bitCast(floating_point)) });
        }

        try result_writer.writeAll(self.assembly.text_section.items);
    }

    if (self.assembly.data_section.items.len > 0) {
        try result_writer.print(".section \".data\"\n", .{});
        try result_writer.writeAll(self.assembly.data_section.items);
    }

    self.assembly.deinit();

    return try result.toOwnedSlice();
}
