const std = @import("std");

const Ir = @import("Ir.zig");

const Assembly = @This();

text_section: std.ArrayList(u8),
data_section: std.ArrayList(u8),
rodata_section: std.ArrayList(u8),

pub fn init(allocator: std.mem.Allocator) Assembly {
    return Assembly{
        .text_section = std.ArrayList(u8).init(allocator),
        .data_section = std.ArrayList(u8).init(allocator),
        .rodata_section = std.ArrayList(u8).init(allocator),
    };
}

pub fn deinit(self: Assembly) void {
    self.text_section.deinit();
    self.data_section.deinit();
    self.rodata_section.deinit();
}

pub const Aarch64 = struct {
    allocator: std.mem.Allocator,

    assembly: Assembly,

    ir: Ir,

    stack: std.ArrayList(RegisterInfo),
    stack_offsets: std.ArrayList(usize),
    stack_map: std.StringHashMap(usize),
    stack_alignment: usize = 16,
    stack_size: usize = 0,

    const RegisterInfo = struct {
        prefix: u8,
    };

    pub fn init(allocator: std.mem.Allocator, ir: Ir) Aarch64 {
        return Aarch64{
            .allocator = allocator,
            .assembly = Assembly.init(allocator),
            .ir = ir,
            .stack = std.ArrayList(RegisterInfo).init(allocator),
            .stack_offsets = std.ArrayList(usize).init(allocator),
            .stack_map = std.StringHashMap(usize).init(allocator),
        };
    }

    pub const Error = std.mem.Allocator.Error;

    pub fn render(self: *Aarch64) Error!void {
        const text_section_writer = self.assembly.text_section.writer();
        const rodata_section_writer = self.assembly.rodata_section.writer();

        for (self.ir.instructions, 0..) |instruction, i| {
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
                    self.stack_size = self.stack_alignment;

                    for (self.ir.instructions[i..]) |other_instruction| {
                        if (other_instruction == .store) {
                            self.stack_size += self.stack_alignment;
                        } else if (other_instruction == .function_epilogue) {
                            break;
                        }
                    }

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

    fn pushRegister(self: *Aarch64, register_number: u8, register_info: RegisterInfo) Error!void {
        try self.stack.append(register_info);

        const stack_offset = self.stack_offsets.items[self.stack_offsets.items.len - 1] + self.stack_alignment;

        try self.stack_offsets.append(stack_offset);

        const text_section_writer = self.assembly.text_section.writer();

        try text_section_writer.print("\tstr {c}{}, [x29, #{}]\n", .{ register_info.prefix, register_number, stack_offset });
    }

    fn popRegister(self: *Aarch64, register_number: u8) Error!void {
        const register_info = self.stack.pop();

        const stack_offset = self.stack_offsets.pop();

        const text_section_writer = self.assembly.text_section.writer();

        try text_section_writer.print("\tldr {c}{}, [x29, #{}] \n", .{ register_info.prefix, register_number, stack_offset });
    }

    pub fn dump(self: *Aarch64) Error![]const u8 {
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
};

pub const X86_64 = struct {
    assembly: Assembly,

    ir: Ir,

    stack: std.ArrayList(RegisterInfo),
    stack_offsets: std.ArrayList(usize),
    stack_map: std.StringHashMap(usize),
    stack_alignment: usize = 16,

    floating_points: std.ArrayList(f64),

    allocator: std.mem.Allocator,

    const RegisterInfo = struct {
        floating_point: bool,
    };

    pub fn init(allocator: std.mem.Allocator, ir: Ir) X86_64 {
        return X86_64{
            .allocator = allocator,
            .assembly = Assembly.init(allocator),
            .ir = ir,
            .stack = std.ArrayList(RegisterInfo).init(allocator),
            .stack_offsets = std.ArrayList(usize).init(allocator),
            .stack_map = std.StringHashMap(usize).init(allocator),
            .floating_points = std.ArrayList(f64).init(allocator),
        };
    }

    pub const Error = std.mem.Allocator.Error;

    pub fn render(self: *X86_64) Error!void {
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

                        try self.copyFromStack("8", register_info, self.stack_offsets.items[stack_loc + 1]);

                        try self.pushRegister("8", register_info);
                    },

                    .string => {
                        try rodata_section_writer.print("\tstr{}: .asciz \"{s}\"\n", .{ instruction.load.string, self.ir.string_literals[instruction.load.string] });
                        try text_section_writer.print("\tleaq str{}(%rip), %r8\n", .{instruction.load.string});

                        try self.pushRegister("8", .{ .floating_point = false });
                    },

                    .char => {
                        try text_section_writer.print("\tmovq ${}, %r8\n", .{instruction.load.char});

                        try self.pushRegister("8", .{ .floating_point = false });
                    },

                    .int => {
                        try text_section_writer.print("\tmovq ${}, %r8\n", .{instruction.load.int});

                        try self.pushRegister("8", .{ .floating_point = false });
                    },

                    .float => {
                        try self.floating_points.append(instruction.load.float);

                        try text_section_writer.print("\tmovsd flt{}, %xmm8\n", .{self.floating_points.items.len - 1});

                        try self.pushRegister("8", .{ .floating_point = true });
                    },
                },

                .label => {
                    try text_section_writer.print(".global {s}\n", .{instruction.label.name});
                    try text_section_writer.print("{s}:\n", .{instruction.label.name});
                },

                .function_proluge => {
                    try text_section_writer.print("\tpushq %rbp\n", .{});
                    try text_section_writer.print("\tmovq %rsp, %rbp\n", .{});

                    try self.stack_offsets.append(0);
                },

                .function_epilogue => {
                    if (self.stack.items.len != 0) {
                        try self.popRegister("ax");
                    }

                    try text_section_writer.print("\tpopq %rbp\n", .{});

                    self.stack.clearAndFree();
                    self.stack_offsets.clearAndFree();
                    self.stack_map.clearAndFree();
                },

                .inline_assembly => {
                    try text_section_writer.print("\t{s}\n", .{instruction.inline_assembly.content});
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
        } else {
            return register_suffix[0] - '0';
        }
    }

    fn pushRegister(self: *X86_64, register_suffix: []const u8, register_info: RegisterInfo) Error!void {
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

    fn copyFromStack(self: *X86_64, register_suffix: []const u8, register_info: RegisterInfo, stack_offset: usize) Error!void {
        const text_section_writer = self.assembly.text_section.writer();

        if (register_info.floating_point) {
            try text_section_writer.print("\tmovsd -{}(%rbp), %xmm{}\n", .{ stack_offset, suffixToNumber(register_suffix) });
        } else {
            try text_section_writer.print("\tmovq -{}(%rbp), %r{s}\n", .{ stack_offset, register_suffix });
        }
    }

    fn popRegister(self: *X86_64, register_suffix: []const u8) Error!void {
        const register_info = self.stack.pop();

        const stack_offset = self.stack_offsets.pop();

        try self.copyFromStack(register_suffix, register_info, stack_offset);
    }

    pub fn dump(self: *X86_64) Error![]const u8 {
        var result = std.ArrayList(u8).init(self.allocator);

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

        if (self.assembly.rodata_section.items.len > 0) {
            try result_writer.print(".section \".rodata\"\n", .{});
            try result_writer.writeAll(self.assembly.rodata_section.items);
        }

        self.assembly.deinit();

        return try result.toOwnedSlice();
    }
};
