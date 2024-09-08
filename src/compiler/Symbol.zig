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

    symbols: std.ArrayListUnmanaged(Symbol) = .{},

    pub fn init(allocator: std.mem.Allocator) Table {
        return Table{ .allocator = allocator };
    }

    pub fn set(self: *Table, symbol: Symbol) std.mem.Allocator.Error!void {
        var swapped = false;

        for (self.symbols.items, 0..) |other, i| {
            if (std.mem.eql(u8, symbol.name.buffer, other.name.buffer)) {
                self.symbols.items[i] = symbol;

                swapped = true;
            }
        }

        if (!swapped) {
            try self.symbols.append(self.allocator, symbol);
        }
    }

    pub fn reset(self: *Table) void {
        var i: usize = 0;

        while (i < self.symbols.items.len) {
            const symbol = self.symbols.items[i];

            if (symbol.linkage == .local) {
                _ = self.symbols.swapRemove(i);
            } else {
                i += 1;
            }
        }
    }

    const LookupError = error{Undeclared};

    pub fn lookup(self: Table, name: []const u8) LookupError!Symbol {
        for (self.symbols.items) |symbol| {
            if (std.mem.eql(u8, symbol.name.buffer, name)) {
                return symbol;
            }
        }

        return error.Undeclared;
    }
};
