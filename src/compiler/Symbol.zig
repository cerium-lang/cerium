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

pub const Table = struct {
    allocator: std.mem.Allocator,

    original: std.StringHashMapUnmanaged(Symbol) = .{},
    modified: std.StringHashMapUnmanaged(Symbol) = .{},

    pub fn init(allocator: std.mem.Allocator) Table {
        return Table{ .allocator = allocator };
    }

    pub fn put(self: *Table, symbol: Symbol) std.mem.Allocator.Error!void {
        try self.modified.put(self.allocator, symbol.name.buffer, symbol);

        if (symbol.linkage == .global) {
            try self.original.put(self.allocator, symbol.name.buffer, symbol);
        }
    }

    pub fn get(self: Table, name: []const u8) ?Symbol {
        if (self.modified.get(name)) |symbol| {
            return symbol;
        } else if (self.original.get(name)) |symbol| {
            return symbol;
        }

        return null;
    }

    pub fn clearAndFree(self: *Table) void {
        self.modified.clearAndFree(self.allocator);
    }
};
