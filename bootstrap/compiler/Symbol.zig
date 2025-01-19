const std = @import("std");

const Name = @import("Sir.zig").Name;
const Compilation = @import("Compilation.zig");

const Symbol = @This();

tag: Tag,
name: Name,
type: Type,

pub const Tag = enum {
    local,
    global,
    builtin,
};

pub const Type = union(enum) {
    void,
    bool,
    int: Int,
    float: Float,
    pointer: Pointer,
    function: Function,
    @"struct": Struct,
    array: Array,

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
            slice,
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

    pub const Array = struct {
        len: usize,
        child_type: *const Type,
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

    var string_array_types: std.AutoHashMapUnmanaged(usize, Type) = .{};

    pub fn string(len: usize) Type {
        const array_type_on_heap = blk: {
            const array_type_entry = string_array_types.getOrPutValue(
                std.heap.page_allocator, // We use page allocator beacuse passing an allocator in here
                // would break in multiple places, and painful to fix
                len,
                .{
                    .array = .{
                        .len = len,
                        .child_type = &.{ .int = .{ .signedness = .unsigned, .bits = 8 } },
                    },
                },
            ) catch @panic("OOM"); // This is not ideal to handle out of memory

            break :blk array_type_entry.value_ptr;
        };

        return Type{
            .pointer = .{
                .size = .one,
                .is_const = true,
                .child_type = array_type_on_heap,
            },
        };
    }

    pub fn canBeNegative(self: Type) bool {
        return switch (self) {
            .float => true,
            .int => |int| int.signedness == .signed,

            else => false,
        };
    }

    pub fn getPointer(self: Type) ?Type.Pointer {
        if (self != .pointer) return null;
        return self.pointer;
    }

    pub fn getArray(self: Type) ?Type.Array {
        if (self != .array) return null;
        return self.array;
    }

    pub fn getStruct(self: Type) ?Type.Struct {
        if (self != .@"struct") return null;
        return self.@"struct";
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

            .int => |int| try writer.print("{c}{}", .{ if (int.signedness == .unsigned) @as(u8, 'u') else @as(u8, 's'), int.bits }),
            .float => |float| try writer.print("f{}", .{float.bits}),

            .array => |array| try writer.print("[{}]{}", .{ array.len, array.child_type }),

            .pointer => |pointer| {
                if (pointer.size == .one) {
                    try writer.writeAll("*");
                } else if (pointer.size == .many) {
                    try writer.writeAll("[*]");
                } else if (pointer.size == .slice) {
                    try writer.writeAll("[]");
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
                    var recursive = false;

                    if (field.type.getPointer()) |pointer| {
                        if (pointer.child_type.getStruct()) |child_struct| {
                            for (child_struct.fields) |child_field|
                                if ((child_field.type == .pointer and child_field.type.pointer.child_type.eql(self)) or
                                    child_field.type.eql(self))
                                {
                                    recursive = true;
                                    break;
                                };
                        } else if (pointer.child_type.eql(self)) {
                            recursive = true;
                        }
                    }

                    if (recursive) {
                        try writer.print("{s} ", .{field.name});

                        const pointer = field.type.pointer;

                        if (pointer.size == .one) {
                            try writer.writeAll("*");
                        } else if (pointer.size == .many) {
                            try writer.writeAll("[*]");
                        }

                        if (pointer.is_const) {
                            try writer.writeAll("const ");
                        }

                        try writer.writeAll("{...}");
                    } else {
                        try writer.print("{s} {}", .{ field.name, field.type });
                    }

                    if (i < @"struct".fields.len - 1) {
                        try writer.writeAll(", ");
                    }
                }

                try writer.writeAll(" }");
            },
        }
    }

    pub fn eql(self: Type, other: Type) bool {
        if (self.getPointer()) |pointer| {
            const other_pointer = other.getPointer() orelse return false;

            return ((!pointer.is_const and other_pointer.is_const) or pointer.is_const == other_pointer.is_const) and
                pointer.size == other_pointer.size and
                pointer.child_type.eql(other_pointer.child_type.*);
        } else if (self.getFunction()) |function| {
            const other_function = other.getFunction() orelse return false;

            if (function.parameter_types.len != other_function.parameter_types.len) return false;

            for (function.parameter_types, other_function.parameter_types) |parameter, other_parameter|
                if (!parameter.eql(other_parameter)) return false;

            return function.return_type.eql(other_function.return_type.*);
        } else if (self.getArray()) |array| {
            const other_array = other.getArray() orelse return false;

            return array.len == other_array.len and
                array.child_type.eql(other_array.child_type.*);
        } else if (self.getStruct()) |@"struct"| {
            const other_struct = other.getStruct() orelse return false;

            if (@"struct".fields.len != other_struct.fields.len) return false;

            for (@"struct".fields, other_struct.fields) |field, other_field| {
                if (field.type.getPointer()) |pointer|
                    if (other_field.type.getPointer()) |other_pointer| {
                        if (pointer.child_type == other_pointer.child_type)
                            continue;
                    } else {
                        return false;
                    };

                if (!field.type.eql(other_field.type)) return false;
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

        pub fn clearRetainingCapacity(self: *Self) void {
            self.items.clearRetainingCapacity();
        }

        pub fn deinit(self: *Self, allocator: std.mem.Allocator) void {
            self.items.deinit(allocator);
        }

        pub fn iterator(self: *const Self) @FieldType(Self, "items").Iterator {
            return self.items.iterator();
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
