const std = @import("std");

const Lir = @import("Lir.zig");

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

    lir: Lir,

    string_literals_index: usize = 0,

    stack: std.ArrayList(RegisterInfo),
    stack_offsets: std.ArrayList(usize),
    stack_map: std.StringHashMap(usize),
    stack_alignment: usize = 16,
    stack_size: usize = 0,

    const RegisterInfo = struct {
        prefix: u8,
    };

    pub fn init(allocator: std.mem.Allocator, lir: Lir) Aarch64 {
        return Aarch64{
            .allocator = allocator,
            .assembly = Assembly.init(allocator),
            .lir = lir,
            .stack = std.ArrayList(RegisterInfo).init(allocator),
            .stack_offsets = std.ArrayList(usize).init(allocator),
            .stack_map = std.StringHashMap(usize).init(allocator),
        };
    }

    pub const Error = std.mem.Allocator.Error;

    pub fn render(self: *Aarch64) Error!void {
        const text_section_writer = self.assembly.text_section.writer();
        const rodata_section_writer = self.assembly.rodata_section.writer();

        for (self.lir.instructions.items, 0..) |instruction, i| {
            switch (instruction) {
                .label => |label| {
                    try text_section_writer.print(".global {s}\n", .{label});
                    try text_section_writer.print("{s}:\n", .{label});
                },

                .function_proluge => {
                    var allocated_variables: std.StringHashMapUnmanaged(void) = .{};
                    defer allocated_variables.deinit(self.allocator);

                    for (self.lir.instructions.items[i..]) |other_instruction| {
                        if (other_instruction == .set) {
                            try allocated_variables.put(self.allocator, other_instruction.set, {});
                        } else if (other_instruction == .function_epilogue) {
                            break;
                        }
                    }

                    self.stack_size = (allocated_variables.count() + 1) * self.stack_alignment;

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

                .set => |name| {
                    if (self.stack_map.get(name)) |stack_loc| {
                        const register_info = self.stack.items[stack_loc];

                        try self.popRegister(8);

                        try text_section_writer.print("\tstr {c}8, [x29, #{}]\n", .{ register_info.prefix, self.stack_offsets.items[stack_loc + 1] });
                    } else {
                        try self.stack_map.put(name, self.stack.items.len - 1);
                    }
                },

                .get => |name| {
                    const stack_loc = self.stack_map.get(name).?;

                    const register_info = self.stack.items[stack_loc];

                    try text_section_writer.print("\tldr {c}8, [x29, #{}]\n", .{ register_info.prefix, self.stack_offsets.items[stack_loc + 1] });

                    try self.pushRegister(8, register_info);
                },

                .string => |string| {
                    try rodata_section_writer.print("\tstr{}: .asciz \"{s}\"\n", .{ self.string_literals_index, string });
                    try text_section_writer.print("\tadr x8, str{}\n", .{self.string_literals_index});

                    self.string_literals_index += 1;

                    try self.pushRegister(8, .{ .prefix = 'x' });
                },

                .int => |int| {
                    try text_section_writer.print("\tmov x8, #{}\n", .{int});

                    try self.pushRegister(8, .{ .prefix = 'x' });
                },

                .float => |float| {
                    try text_section_writer.print("\tfmov d8, #{}\n", .{float});

                    try self.pushRegister(8, .{ .prefix = 'd' });
                },

                .negate => {
                    const register_info = self.stack.getLast();

                    try self.popRegister(9);

                    if (register_info.prefix == 'd') {
                        try text_section_writer.print("\tfneg d10, d9\n", .{});
                    } else {
                        try text_section_writer.print("\tmov {}8, {}zr\n", .{ register_info.prefix, register_info.prefix });
                        try text_section_writer.print("\tsubs {}10, {}8, {}9\n", .{ register_info.prefix, register_info.prefix, register_info.prefix });
                    }

                    try self.pushRegister(10, register_info);
                },

                .pop => {
                    try self.popRegister(8);
                },

                .@"asm" => |content| {
                    try text_section_writer.print("\t{s}\n", .{content});
                },

                .@"return" => {
                    try text_section_writer.print("\tret\n", .{});
                },
            }
        }
    }

    fn pushRegister(self: *Aarch64, register_number: u8, register_info: RegisterInfo) Error!void {
        try self.stack.append(register_info);

        const stack_offset = self.stack_offsets.getLast() + self.stack_alignment;

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
    allocator: std.mem.Allocator,

    assembly: Assembly,

    lir: Lir,

    stack: std.ArrayList(RegisterInfo),
    stack_offsets: std.ArrayList(usize),
    stack_map: std.StringHashMap(usize),
    stack_alignment: usize = 16,

    string_literals_index: usize = 0,
    floating_points_index: usize = 0,

    const RegisterInfo = struct {
        floating_point: bool,
    };

    pub fn init(allocator: std.mem.Allocator, lir: Lir) X86_64 {
        return X86_64{
            .allocator = allocator,
            .assembly = Assembly.init(allocator),
            .lir = lir,
            .stack = std.ArrayList(RegisterInfo).init(allocator),
            .stack_offsets = std.ArrayList(usize).init(allocator),
            .stack_map = std.StringHashMap(usize).init(allocator),
        };
    }

    pub const Error = std.mem.Allocator.Error;

    pub fn render(self: *X86_64) Error!void {
        const text_section_writer = self.assembly.text_section.writer();
        const rodata_section_writer = self.assembly.rodata_section.writer();

        for (self.lir.instructions.items) |instruction| {
            switch (instruction) {
                .label => |label| {
                    try text_section_writer.print(".global {s}\n", .{label});
                    try text_section_writer.print("{s}:\n", .{label});
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

                .set => |name| {
                    if (self.stack_map.get(name)) |stack_loc| {
                        const register_info = self.stack.items[stack_loc];

                        try self.popRegister("8");

                        try self.copyToStack("8", register_info, self.stack_offsets.items[stack_loc + 1]);
                    } else {
                        try self.stack_map.put(name, self.stack.items.len - 1);
                    }
                },

                .get => |name| {
                    const stack_loc = self.stack_map.get(name).?;

                    const register_info = self.stack.items[stack_loc];

                    try self.copyFromStack("8", register_info, self.stack_offsets.items[stack_loc + 1]);

                    try self.pushRegister("8", register_info);
                },

                .string => |string| {
                    try rodata_section_writer.print("\tstr{}: .asciz \"{s}\"\n", .{ self.string_literals_index, string });
                    try text_section_writer.print("\tleaq str{}(%rip), %r8\n", .{self.string_literals_index});

                    self.string_literals_index += 1;

                    try self.pushRegister("8", .{ .floating_point = false });
                },

                .int => |int| {
                    try text_section_writer.print("\tmovq ${}, %r8\n", .{int});

                    try self.pushRegister("8", .{ .floating_point = false });
                },

                .float => |float| {
                    try rodata_section_writer.print("flt{}: .quad {}\n", .{ self.floating_points_index, @as(u64, @bitCast(float)) });
                    try text_section_writer.print("\tmovsd flt{}(%rip), %xmm8\n", .{self.floating_points_index});

                    self.floating_points_index += 1;

                    try self.pushRegister("8", .{ .floating_point = true });
                },

                .negate => {
                    const register_info = self.stack.getLast();

                    try self.popRegister("bx");

                    if (register_info.floating_point) {
                        try text_section_writer.print("\tmovq %xmm1, %rbx\n", .{});
                        try text_section_writer.print("\tmovabsq $0x8000000000000000, %rax\n", .{});
                        try text_section_writer.print("\txorq %rax, %rbx\n", .{});
                        try text_section_writer.print("\tmovsd %rbx, %xmm1\n", .{});
                    } else {
                        try text_section_writer.print("\txorq %rax, %rax\n", .{});
                        try text_section_writer.print("\tsubq %rax, %rbx\n", .{});
                    }

                    try self.pushRegister("bx", register_info);
                },

                .pop => {
                    try self.popRegister("8");
                },

                .@"asm" => |content| {
                    try text_section_writer.print("\t{s}\n", .{content});
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
        } else {
            return register_suffix[0] - '0';
        }
    }

    fn pushRegister(self: *X86_64, register_suffix: []const u8, register_info: RegisterInfo) Error!void {
        try self.stack.append(register_info);

        const stack_offset = self.stack_offsets.getLast() + self.stack_alignment;

        try self.stack_offsets.append(stack_offset);

        try self.copyToStack(register_suffix, register_info, stack_offset);
    }

    fn copyFromStack(self: *X86_64, register_suffix: []const u8, register_info: RegisterInfo, stack_offset: usize) Error!void {
        const text_section_writer = self.assembly.text_section.writer();

        if (register_info.floating_point) {
            try text_section_writer.print("\tmovsd -{}(%rbp), %xmm{}\n", .{ stack_offset, suffixToNumber(register_suffix) });
        } else {
            try text_section_writer.print("\tmovq -{}(%rbp), %r{s}\n", .{ stack_offset, register_suffix });
        }
    }

    fn copyToStack(self: *X86_64, register_suffix: []const u8, register_info: RegisterInfo, stack_offset: usize) Error!void {
        const text_section_writer = self.assembly.text_section.writer();

        if (register_info.floating_point) {
            try text_section_writer.print("\tmovsd %xmm{}, -{}(%rbp)\n", .{ suffixToNumber(register_suffix), stack_offset });
        } else {
            try text_section_writer.print("\tmovq %r{s}, -{}(%rbp)\n", .{ register_suffix, stack_offset });
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
