const std = @import("std");

const Name = @import("ast.zig").Name;
const Type = @import("type.zig").Type;

const SymbolTable = @This();

symbols: std.ArrayList(Symbol),

pub const Symbol = struct {
    name: Name,
    type: Type,
    linkage: Linkage,

    pub const Linkage = enum { internal, external };
};

pub fn init(gpa: std.mem.Allocator) SymbolTable {
    return SymbolTable{ .symbols = std.ArrayList(Symbol).init(gpa) };
}

const SetError = error{Redeclaration} || std.mem.Allocator.Error;

pub fn set(self: *SymbolTable, symbol: Symbol) SetError!void {
    if (self.lookup(symbol.name.buffer) != error.Undeclared) {
        return error.Redeclaration;
    } else {
        return self.symbols.append(symbol);
    }
}

pub fn reset(self: *SymbolTable) void {
    for (self.symbols.items, 0..) |symbol, i| {
        if (symbol.linkage != .external) {
            _ = self.symbols.swapRemove(i);
        }
    }
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
