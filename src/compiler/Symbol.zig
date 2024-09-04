const std = @import("std");

const Name = @import("Ast.zig").Name;
const Type = @import("Type.zig");

const Symbol = @This();

name: Name,
type: Type,

pub const Table = struct {
    symbols: std.ArrayList(Symbol),

    pub fn init(allocator: std.mem.Allocator) Table {
        return Table{
            .symbols = std.ArrayList(Symbol).init(allocator),
        };
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
            try self.symbols.append(symbol);
        }
    }

    pub fn reset(self: *Table) void {
        self.symbols.clearAndFree();
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
