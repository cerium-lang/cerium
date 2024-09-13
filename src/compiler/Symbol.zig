const std = @import("std");

const Name = @import("Ast.zig").Name;
const Type = @import("Type.zig");

const Symbol = @This();

name: Name,
type: Type,
linkage: Linkage,

pub const Linkage = enum {
    local,
    global,
};

pub fn Scope(comptime V: type) type {
    return struct {
        const Self = @This();

        maybe_parent: ?*Self = null,

        items: std.StringHashMapUnmanaged(V) = .{},

        pub fn put(self: *Self, allocator: std.mem.Allocator, name: []const u8, value: V) std.mem.Allocator.Error!void {
            try self.items.put(allocator, name, value);
        }

        pub fn get(self: Self, name: []const u8) ?V {
            if (self.items.get(name)) |value| {
                return value;
            }

            if (self.maybe_parent) |parent| {
                return parent.get(name);
            }

            return null;
        }

        pub fn getPtr(self: *Self, name: []const u8) ?*V {
            if (self.items.getPtr(name)) |value| {
                return value;
            }

            if (self.maybe_parent) |parent| {
                return parent.getPtr(name);
            }

            return null;
        }

        pub fn clearAndFree(self: *Self, allocator: std.mem.Allocator) void {
            self.items.clearAndFree(allocator);
        }
    };
}
