const std = @import("std");

const Name = @import("Hir.zig").Name;
const Compilation = @import("Compilation.zig");

const Symbol = @This();

name: Name,
type: Type,
linkage: Linkage,

pub const Linkage = enum {
    local,
    global,
    external,
};

pub const Type = union(enum) {
    void,
    bool,
    ambigiuous_int,
    ambigiuous_float,
    int: Int,
    float: Float,
    pointer: Pointer,
    function: Function,
    @"struct": Struct,

    pub const Int = struct {
        signedness: Signedness,
        bits: u16,

        pub const Signedness = enum {
            signed,
            unsigned,
        };
    };

    pub const Float = struct {
        bits: u16,
    };

    pub const Pointer = struct {
        size: Size,
        is_const: bool,
        is_local: bool,
        child_type: *const Type,

        pub const Size = enum {
            one,
            many,
        };
    };

    pub const Function = struct {
        parameter_types: []const Type,
        return_type: *const Type,
    };

    pub const Struct = struct {
        fields: []const Field,

        pub const Field = struct {
            name: []const u8,
            type: Type,
        };
    };

    pub fn isInt(self: Type) bool {
        return switch (self) {
            .ambigiuous_int, .int => true,

            else => false,
        };
    }

    pub fn isFloat(self: Type) bool {
        return switch (self) {
            .ambigiuous_float, .float => true,

            else => false,
        };
    }

    pub fn isIntOrFloat(self: Type) bool {
        return (self.isInt() or self.isFloat());
    }

    pub fn isIntOrFloatOrPointer(self: Type) bool {
        return (self.isInt() or self.isFloat() or self == .pointer);
    }

    pub fn bitSize(self: Type, env: Compilation.Environment) u16 {
        return switch (self) {
            .void => 0,
            .bool => 1,
            .ambigiuous_int => 128,
            .ambigiuous_float => 64,
            .pointer => env.target.ptrBitWidth(),
            .function => env.target.ptrBitWidth(),

            inline else => |other| other.bits,
        };
    }

    pub fn byteSize(self: Type, env: Compilation.Environment) u16 {
        return std.math.divCeil(u16, self.bitSize(env), 8) catch unreachable;
    }

    pub fn isLocalPointer(self: Type) bool {
        return self == .pointer and self.pointer.is_local;
    }

    pub fn minInt(self: Type) i128 {
        return switch (self) {
            .ambigiuous_int => std.math.minInt(i128),

            .int => |int| {
                std.debug.assert(int.bits <= 64);
                if (int.signedness == .unsigned or int.bits == 0) return 0;
                return -(@as(i128, 1) << @intCast(int.bits - 1));
            },

            else => unreachable,
        };
    }

    pub fn maxInt(self: Type) i128 {
        return switch (self) {
            .ambigiuous_int => std.math.maxInt(i128),
            .int => |int| {
                std.debug.assert(int.bits <= 64);
                if (int.bits == 0) return 0;
                return (@as(i128, 1) << @intCast(int.bits - @intFromBool(int.signedness == .signed))) - 1;
            },

            else => unreachable,
        };
    }

    pub fn maxFloat(self: Type) f64 {
        return switch (self) {
            .ambigiuous_float => std.math.floatMax(f64),
            .float => |float| if (float.bits == 32) std.math.floatMax(f32) else if (float.bits == 64) std.math.floatMax(f64) else unreachable,

            else => unreachable,
        };
    }

    pub fn canBeNegative(self: Type) bool {
        return switch (self) {
            .ambigiuous_int, .ambigiuous_float => true,
            .float => true,
            .int => |int| int.signedness == .signed,

            else => false,
        };
    }

    pub fn isAmbigiuous(self: Type) bool {
        return switch (self) {
            .ambigiuous_int, .ambigiuous_float => true,

            else => false,
        };
    }

    pub fn getPointer(self: Type) ?Type.Pointer {
        if (self != .pointer) {
            return null;
        }

        return self.pointer;
    }

    pub fn getFunction(self: Type) ?Type.Function {
        if (self.getPointer()) |pointer| {
            if (pointer.child_type.* == .function) {
                return pointer.child_type.function;
            } else {
                return null;
            }
        }

        if (self != .function) {
            return null;
        }

        return self.function;
    }

    pub fn format(self: Type, _: anytype, _: anytype, writer: anytype) !void {
        switch (self) {
            .void => try writer.writeAll("void"),
            .bool => try writer.writeAll("bool"),

            .pointer => |pointer| {
                if (pointer.size == .one) {
                    try writer.writeAll("*");
                } else if (pointer.size == .many) {
                    try writer.writeAll("[*]");
                }

                if (pointer.is_const) {
                    try writer.writeAll("const ");
                }

                try writer.print("{}", .{pointer.child_type});
            },

            .function => |function| {
                try writer.writeAll("fn (");

                for (function.parameter_types, 0..) |parameter, i| {
                    try writer.print("{}", .{parameter});

                    if (i < function.parameter_types.len - 1) {
                        try writer.writeAll(", ");
                    }
                }

                try writer.print(") {}", .{function.return_type});
            },

            .@"struct" => |@"struct"| {
                try writer.writeAll("struct { ");

                for (@"struct".fields, 0..) |field, i| {
                    try writer.print("{}", .{field.type});

                    if (i < @"struct".fields.len - 1) {
                        try writer.writeAll(", ");
                    }
                }

                try writer.writeAll(" }");
            },

            .ambigiuous_int => try writer.writeAll("ambigiuous_int"),
            .ambigiuous_float => try writer.writeAll("ambigiuous_float"),

            .int => |int| try writer.print("{c}{}", .{ if (int.signedness == .unsigned) @as(u8, 'u') else @as(u8, 'i'), int.bits }),
            .float => |float| try writer.print("f{}", .{float.bits}),
        }
    }

    pub fn eql(self: Type, other: Type) bool {
        if (self.getPointer()) |pointer| {
            const other_pointer = other.getPointer() orelse return false;

            if (!pointer.child_type.eql(other_pointer.child_type.*) or
                (pointer.is_const and !other_pointer.is_const) or
                pointer.size != other_pointer.size)
            {
                return false;
            }

            return true;
        } else if (self.getFunction()) |function| {
            const other_function = other.getFunction() orelse return false;

            if (function.parameter_types.len != other_function.parameter_types.len) {
                return false;
            }

            for (function.parameter_types, other_function.parameter_types) |parameter, other_parameter| {
                if (!parameter.eql(other_parameter)) {
                    return false;
                }
            }

            if (!function.return_type.eql(other_function.return_type.*)) {
                return false;
            }

            return true;
        } else {
            return std.meta.eql(self, other);
        }
    }
};

pub fn Scope(comptime V: type) type {
    return struct {
        const Self = @This();

        maybe_parent: ?*Self = null,

        items: std.StringHashMapUnmanaged(V) = .{},

        pub fn put(self: *Self, allocator: std.mem.Allocator, name: []const u8, value: V) std.mem.Allocator.Error!void {
            try self.items.put(allocator, name, value);
        }

        pub fn putAssumeCapacity(self: *Self, name: []const u8, value: V) void {
            self.items.putAssumeCapacity(name, value);
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

        pub fn ensureTotalCapacity(self: *Self, allocator: std.mem.Allocator, new_capacity: u32) std.mem.Allocator.Error!void {
            try self.items.ensureTotalCapacity(allocator, new_capacity);
        }
    };
}
