const std = @import("std");

const Assembly = @This();

text_section: std.ArrayList(u8),
data_section: std.ArrayList(u8),

pub fn init(gpa: std.mem.Allocator) Assembly {
    return Assembly{ .text_section = std.ArrayList(u8).init(gpa), .data_section = std.ArrayList(u8).init(gpa) };
}

pub fn deinit(self: Assembly) void {
    self.text_section.deinit();
    self.data_section.deinit();
}
