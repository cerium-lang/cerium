const std = @import("std");
const IR = @import("../IR.zig");
const Assembly = @import("../Assembly.zig");
const Aarch64Renderer = @This();

assembly: Assembly,

ir: IR,

stack: std.ArrayList(RegisterInfo),
stack_offsets: std.ArrayList(usize),
stack_alignment: usize = 16,
// Stack size has to be calculated before rendering using "how many variables we need", not just a random constant number like this
stack_size: usize = 256,

variables: std.StringHashMap(VariableInfo),

gpa: std.mem.Allocator,

const RegisterInfo = struct {
    prefix: u8,
};

const VariableInfo = struct { stack_loc: usize };

pub fn init(gpa: std.mem.Allocator, ir: IR) Aarch64Renderer {
    return Aarch64Renderer{ .assembly = Assembly.init(gpa), .ir = ir, .stack = std.ArrayList(RegisterInfo).init(gpa), .stack_offsets = std.ArrayList(usize).init(gpa), .variables = std.StringHashMap(VariableInfo).init(gpa), .gpa = gpa };
}

pub fn render(self: *Aarch64Renderer) std.mem.Allocator.Error!void {
    const text_section_writer = self.assembly.text_section.writer();

    for (self.ir.instructions) |instruction| {
        switch (instruction) {
            .store => {
                try self.pushValue(instruction.store.value);

                try self.variables.put(instruction.store.name, .{ .stack_loc = self.stack.items.len - 1 });
            },

            .load => {
                try self.pushValue(instruction.load.value);
            },

            .label => {
                try text_section_writer.print(".global {s}\n", .{instruction.label.name});
                try text_section_writer.print("{s}:\n", .{instruction.label.name});

                try self.functionProluge();
            },

            .ret => {
                if (self.stack.items.len != 0) {
                    try self.popRegister(0);
                }

                try self.functionEpilogue();

                try text_section_writer.print("\tret\n", .{});
            },
        }
    }
}

fn functionProluge(self: *Aarch64Renderer) std.mem.Allocator.Error!void {
    const text_section_writer = self.assembly.text_section.writer();

    try text_section_writer.print("\tsub sp, sp, #{}\n", .{self.stack_size});
    try text_section_writer.print("\tstp x29, x30, [sp, #{}]\n", .{self.stack_alignment});
    try text_section_writer.print("\tadd x29, sp, #{}\n", .{self.stack_alignment});

    try self.stack_offsets.append(0);
}

fn functionEpilogue(self: *Aarch64Renderer) std.mem.Allocator.Error!void {
    const text_section_writer = self.assembly.text_section.writer();

    try text_section_writer.print("\tldp x29, x30, [sp, #{}]\n", .{self.stack_alignment});
    try text_section_writer.print("\tadd sp, sp, #{}\n", .{self.stack_size});

    self.stack.clearAndFree();
    self.stack_offsets.clearAndFree();
    self.variables.clearAndFree();
}

fn pushRegister(self: *Aarch64Renderer, register_number: u8, register_info: RegisterInfo) std.mem.Allocator.Error!void {
    try self.stack.append(register_info);

    const stack_offset = self.stack_offsets.items[self.stack_offsets.items.len - 1] + self.stack_alignment;

    try self.stack_offsets.append(stack_offset);

    const text_section_writer = self.assembly.text_section.writer();

    try text_section_writer.print("\tstr {c}{}, [x29, #{}]\n", .{ register_info.prefix, register_number, stack_offset });
}

fn popRegister(self: *Aarch64Renderer, register_number: u8) std.mem.Allocator.Error!void {
    const register_info = self.stack.pop();

    const stack_offset = self.stack_offsets.pop();

    const text_section_writer = self.assembly.text_section.writer();

    try text_section_writer.print("\tldr {c}{}, [x29, #{}] \n", .{ register_info.prefix, register_number, stack_offset });
}

fn pushValue(self: *Aarch64Renderer, value: IR.Value) std.mem.Allocator.Error!void {
    const text_section_writer = self.assembly.text_section.writer();
    const data_section_writer = self.assembly.data_section.writer();

    switch (value) {
        .variable_reference => {
            const variable_info = self.variables.get(value.variable_reference.name).?;

            const register_info = self.stack.items[variable_info.stack_loc];

            try text_section_writer.print("\tldr {c}8, [x29, #{}]\n", .{ register_info.prefix, self.stack_offsets.items[variable_info.stack_loc + 1] });

            try self.pushRegister(8, register_info);
        },

        .string_reference => {
            try data_section_writer.print("\tstr{}: .asciz \"{s}\"\n", .{ value.string_reference.index, self.ir.string_literals[value.string_reference.index] });
            try text_section_writer.print("\tadr x8, str{}\n", .{value.string_reference.index});

            try self.pushRegister(8, .{ .prefix = 'x' });
        },

        .char => {
            try text_section_writer.print("\tmov w8, #{}\n", .{value.char.value});

            try self.pushRegister(8, .{ .prefix = 'w' });
        },

        .int => {
            try text_section_writer.print("\tmov x8, #{}\n", .{value.int.value});

            try self.pushRegister(8, .{ .prefix = 'x' });
        },

        .float => {
            try text_section_writer.print("\tfmov d8, #{}\n", .{value.float.value});

            try self.pushRegister(8, .{ .prefix = 'd' });
        },
    }
}

pub fn dump(self: *Aarch64Renderer) std.mem.Allocator.Error![]const u8 {
    var result = std.ArrayList(u8).init(self.gpa);

    const w = result.writer();

    if (self.assembly.text_section.items.len > 0) {
        try w.print(".section \".text\"\n", .{});
        try w.writeAll(self.assembly.text_section.items);
    }

    if (self.assembly.data_section.items.len > 0) {
        try w.print(".section \".data\"\n", .{});
        try w.writeAll(self.assembly.data_section.items);
    }

    self.assembly.deinit();

    return try result.toOwnedSlice();
}
