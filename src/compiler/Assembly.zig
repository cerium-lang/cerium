const std = @import("std");

pub const x86_64 = @import("Assembly/x86_64.zig");

const Assembly = @This();

text_section: std.ArrayListUnmanaged(u8) = .{},
data_section: std.ArrayListUnmanaged(u8) = .{},
rodata_section: std.ArrayListUnmanaged(u8) = .{},

pub fn deinit(self: *Assembly, allocator: std.mem.Allocator) void {
    self.text_section.deinit(allocator);
    self.data_section.deinit(allocator);
    self.rodata_section.deinit(allocator);
}
