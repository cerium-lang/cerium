const std = @import("std");

const Name = @import("ast.zig").Name;
const Type = @import("type.zig").Type;

const SymbolTable = @This();

symbols: std.ArrayList(Symbol),

pub const Symbol = struct {
    name: Name,
    type: Type,
};

pub fn init(gpa: std.mem.Allocator) SymbolTable {
    return SymbolTable{ .symbols = std.ArrayList(Symbol).init(gpa) };
}

pub fn set(self: *SymbolTable, symbol: Symbol) std.mem.Allocator.Error!void {
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

pub fn reset(self: *SymbolTable) void {
    self.symbols.clearAndFree();
}

const LookupError = error{Undeclared};

pub fn lookup(self: *const SymbolTable, name: []const u8) LookupError!Symbol {
    for (self.symbols.items) |symbol| {
        if (std.mem.eql(u8, symbol.name.buffer, name)) {
            return symbol;
        }
    }

    return error.Undeclared;
}
