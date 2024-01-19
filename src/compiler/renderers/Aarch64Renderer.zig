const std = @import("std");
const IR = @import("../IR.zig");
const Assembly = @import("../Assembly.zig");
const Aarch64Renderer = @This();

assembly: Assembly,
ir: IR,
gpa: std.mem.Allocator,

pub fn init(gpa: std.mem.Allocator, ir: IR) Aarch64Renderer {
    return Aarch64Renderer{ .assembly = Assembly.init(gpa), .ir = ir, .gpa = gpa };
}

pub fn render(self: *Aarch64Renderer) std.mem.Allocator.Error!void {
    const text_section_writer = self.assembly.text_section.writer();

    for (self.ir.instructions) |instruction| {
        switch (instruction) {
            .label => {
                try text_section_writer.print("{s}:\n", .{instruction.label.name});
            },

            .ret => {
                try self.renderValue(instruction.ret.value, 0);
                try text_section_writer.print("\tret\n", .{});
            },
        }
    }
}

pub fn renderValue(self: *Aarch64Renderer, value: IR.Value, target_register_number: u8) std.mem.Allocator.Error!void {
    const text_section_writer = self.assembly.text_section.writer();
    const data_section_writer = self.assembly.data_section.writer();

    switch (value) {
        .string_reference => {
            try data_section_writer.print("\tstr{}: {s}\n", .{ value.string_reference.index, self.ir.string_literals[value.string_reference.index] });

            try text_section_writer.print("\tadr x{}, str{}\n", .{ target_register_number, value.string_reference.index });
        },

        .char => {
            try text_section_writer.print("\tmov w{}, #{}\n", .{ target_register_number, value.char.value });
        },

        .int => {
            try text_section_writer.print("\tmov x{}, #{}\n", .{ target_register_number, value.int.value });
        },

        .float => {
            try text_section_writer.print("\tfmov d{}, #{}\n", .{ target_register_number, value.float.value });
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
