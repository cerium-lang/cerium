const std = @import("std");

const Name = @import("Sir.zig").Name;
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
    int: Int,
    float: Float,
    pointer: Pointer,
    function: Function,
    @"struct": Struct,

    pub const Int = struct {
        signedness: Signedness,
        bits: u16,

        pub const Signedness = enum {
            unsigned,
            signed,
        };
    };

    pub const Float = struct {
        bits: u16,
    };

    pub const Pointer = struct {
        size: Size,
        is_const: bool,
        child_type: *const Type,

        pub const Size = enum {
            one,
            many,
        };
    };

    pub const Function = struct {
        parameter_types: []const Type,
        is_var_args: bool,
        return_type: *const Type,
    };

    pub const Struct = struct {
        fields: []const Field,

        pub const Field = struct {
            name: []const u8,
            type: Type,
        };
    };

    pub const string: Type = .{
        .pointer = .{
            .size = .many,
            .is_const = true,
            .child_type = &.{ .int = .{ .signedness = .unsigned, .bits = 8 } },
        },
    };

    pub fn minInt(self: Type) i128 {
        return switch (self) {
            .int => |int| {
                if (int.signedness == .unsigned or int.bits == 0) return 0;
                return -(@as(i128, 1) << @intCast(int.bits - 1));
            },

            else => unreachable,
        };
    }

    pub fn maxInt(self: Type) i128 {
        return switch (self) {
            .int => |int| {
                if (int.bits == 0) return 0;
                return (@as(i128, 1) << @intCast(int.bits - @intFromBool(int.signedness == .signed))) - 1;
            },

            else => unreachable,
        };
    }

    pub fn maxFloat(self: Type) f64 {
        return switch (self) {
            .float => |float| if (float.bits == 16)
                std.math.floatMax(f16)
            else if (float.bits == 32)
                std.math.floatMax(f32)
            else if (float.bits == 64)
                std.math.floatMax(f64)
            else
                unreachable,

            else => unreachable,
        };
    }

    pub fn intFittingRange(from: i128, to: i128) Type {
        const signedness: Int.Signedness = @enumFromInt(@intFromBool(from < 0));

        const largest_positive_value = @max(if (from < 0) (-from) - 1 else from, to);

        const base: u7 = @intFromFloat(@ceil(@log2(@as(f64, @floatFromInt(largest_positive_value + 1)))));
        const upper = (@as(i128, 1) << base) - 1;

        var magnitude_bits = if (upper >= largest_positive_value) base else base + 1;
        magnitude_bits += @intFromEnum(signedness);

        return Type{
            .int = .{
                .signedness = signedness,
                .bits = @intCast(magnitude_bits),
            },
        };
    }

    pub fn floatFittingRange(from: f64, to: f64) Type {
        const largest_positive_value = @max(if (from < 0) (-from) - 1 else from, to);

        const base = @ceil(@log2(largest_positive_value + 1));

        return if (base <= 16)
            Type{ .float = .{ .bits = 16 } }
        else if (base <= 32)
            Type{ .float = .{ .bits = 32 } }
        else
            Type{ .float = .{ .bits = 64 } };
    }

    pub fn canBeNegative(self: Type) bool {
        return switch (self) {
            .float => true,
            .int => |int| int.signedness == .signed,

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

            .int => |int| try writer.print("{c}{}", .{ if (int.signedness == .unsigned) @as(u8, 'u') else @as(u8, 's'), int.bits }),
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

        pub fn deinit(self: *Self, allocator: std.mem.Allocator) void {
            self.items.deinit(allocator);
        }

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

        pub fn getOrPut(self: *Self, allocator: std.mem.Allocator, name: []const u8) std.mem.Allocator.Error!std.StringHashMapUnmanaged(V).GetOrPutResult {
            return self.items.getOrPut(allocator, name);
        }

        pub fn ensureTotalCapacity(self: *Self, allocator: std.mem.Allocator, new_capacity: u32) std.mem.Allocator.Error!void {
            try self.items.ensureTotalCapacity(allocator, new_capacity);
        }
    };
}
