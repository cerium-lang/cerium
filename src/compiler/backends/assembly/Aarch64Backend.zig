const std = @import("std");

const IR = @import("../../IR.zig");
const Assembly = @import("Assembly.zig");

const Aarch64Backend = @This();

allocator: std.mem.Allocator,

assembly: Assembly,

ir: IR,

stack: std.ArrayList(RegisterInfo),
stack_offsets: std.ArrayList(usize),
stack_map: std.StringHashMap(usize),
stack_alignment: usize = 16,
// Stack size has to be calculated before rendering using "how many variables we need", not just a random constant number like this
stack_size: usize = 256,

const RegisterInfo = struct {
    prefix: u8,
};

pub fn init(allocator: std.mem.Allocator, ir: IR) Aarch64Backend {
    return Aarch64Backend{
        .allocator = allocator,
        .assembly = Assembly.init(allocator),
        .ir = ir,
        .stack = std.ArrayList(RegisterInfo).init(allocator),
        .stack_offsets = std.ArrayList(usize).init(allocator),
        .stack_map = std.StringHashMap(usize).init(allocator),
    };
}

pub const Error = std.mem.Allocator.Error;

pub fn render(self: *Aarch64Backend) Error!void {
    const text_section_writer = self.assembly.text_section.writer();
    const rodata_section_writer = self.assembly.rodata_section.writer();

    for (self.ir.instructions) |instruction| {
        switch (instruction) {
            .store => {
                try self.stack_map.put(instruction.store.name, self.stack.items.len - 1);
            },

            .load => switch (instruction.load) {
                .name => {
                    const stack_loc = self.stack_map.get(instruction.load.name).?;

                    const register_info = self.stack.items[stack_loc];

                    try text_section_writer.print("\tldr {c}8, [x29, #{}]\n", .{ register_info.prefix, self.stack_offsets.items[stack_loc + 1] });

                    try self.pushRegister(8, register_info);
                },

                .string => {
                    try rodata_section_writer.print("\tstr{}: .asciz \"{s}\"\n", .{ instruction.load.string, self.ir.string_literals[instruction.load.string] });
                    try text_section_writer.print("\tadr x8, str{}\n", .{instruction.load.string});

                    try self.pushRegister(8, .{ .prefix = 'x' });
                },

                .char => {
                    try text_section_writer.print("\tmov w8, #{}\n", .{instruction.load.char});

                    try self.pushRegister(8, .{ .prefix = 'w' });
                },

                .int => {
                    try text_section_writer.print("\tmov x8, #{}\n", .{instruction.load.int});

                    try self.pushRegister(8, .{ .prefix = 'x' });
                },

                .float => {
                    try text_section_writer.print("\tfmov d8, #{}\n", .{instruction.load.float});

                    try self.pushRegister(8, .{ .prefix = 'd' });
                },
            },

            .label => {
                try text_section_writer.print(".global {s}\n", .{instruction.label.name});
                try text_section_writer.print("{s}:\n", .{instruction.label.name});
            },

            .function_proluge => {
                try text_section_writer.print("\tsub sp, sp, #{}\n", .{self.stack_size});
                try text_section_writer.print("\tstp x29, x30, [sp, #{}]\n", .{self.stack_alignment});
                try text_section_writer.print("\tadd x29, sp, #{}\n", .{self.stack_alignment});

                try self.stack_offsets.append(0);
            },

            .function_epilogue => {
                if (self.stack.items.len != 0) {
                    try self.popRegister(0);
                }

                try text_section_writer.print("\tldp x29, x30, [sp, #{}]\n", .{self.stack_alignment});
                try text_section_writer.print("\tadd sp, sp, #{}\n", .{self.stack_size});

                self.stack.clearAndFree();
                self.stack_offsets.clearAndFree();
                self.stack_map.clearAndFree();
            },

            .inline_assembly => {
                try text_section_writer.print("\t{s}\n", .{instruction.inline_assembly.content});
            },

            .@"return" => {
                try text_section_writer.print("\tret\n", .{});
            },
        }
    }
}

fn pushRegister(self: *Aarch64Backend, register_number: u8, register_info: RegisterInfo) Error!void {
    try self.stack.append(register_info);

    const stack_offset = self.stack_offsets.items[self.stack_offsets.items.len - 1] + self.stack_alignment;

    try self.stack_offsets.append(stack_offset);

    const text_section_writer = self.assembly.text_section.writer();

    try text_section_writer.print("\tstr {c}{}, [x29, #{}]\n", .{ register_info.prefix, register_number, stack_offset });
}

fn popRegister(self: *Aarch64Backend, register_number: u8) Error!void {
    const register_info = self.stack.pop();

    const stack_offset = self.stack_offsets.pop();

    const text_section_writer = self.assembly.text_section.writer();

    try text_section_writer.print("\tldr {c}{}, [x29, #{}] \n", .{ register_info.prefix, register_number, stack_offset });
}

pub fn dump(self: *Aarch64Backend) Error![]const u8 {
    var result = std.ArrayList(u8).init(self.allocator);

    const result_writer = result.writer();

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

    self.assembly.deinit();

    return try result.toOwnedSlice();
}
