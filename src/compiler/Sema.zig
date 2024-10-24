//! Semantic Analyzer.
//!
//! An analyzer that lowers down `Sir` to `Air` while checking if the instructions and the types are valid.

const std = @import("std");

const Sir = @import("Sir.zig");
const Name = Sir.Name;
const SourceLoc = Sir.SourceLoc;
const Compilation = @import("Compilation.zig");
const Air = @import("Air.zig");
const Symbol = @import("Symbol.zig");
const Scope = Symbol.Scope;
const Type = Symbol.Type;

const Sema = @This();

allocator: std.mem.Allocator,

env: Compilation.Environment,

sir: Sir,

air: Air,

maybe_function: ?Type = null,

stack: std.ArrayListUnmanaged(Value) = .{},

scope: *Scope(Variable) = undefined,
scope_stack: std.ArrayListUnmanaged(Scope(Variable)) = .{},

error_info: ?ErrorInfo = null,

pub const ErrorInfo = struct {
    message: []const u8,
    source_loc: SourceLoc,
};

pub const Error = error{
    ExpectedCompiletimeConstant,
    UnexpectedFunctionPointer,
    UnexpectedArgumentsCount,
    ExpectedExplicitReturn,
    UnexpectedMutation,
    TypeCannotRepresentValue,
    CircularDependency,
    UnexpectedType,
    MismatchedTypes,
    Undeclared,
    Redeclared,
} || std.mem.Allocator.Error;

const Value = union(enum) {
    string: []const u8,
    int: i128,
    typed_int: TypedInt,
    float: f64,
    boolean: bool,
    runtime: Runtime,

    const TypedInt = struct {
        type: Type,
        value: i128,
    };

    const Runtime = struct {
        type: Type,
        data: Data = .none,

        const Data = union(enum) {
            none,
            name: Name,
        };
    };

    fn canImplicitCast(self: Value, to: Type) bool {
        const self_type = self.getType();

        return (self == .int and to.isInt()) or
            (self == .float and to.isFloat()) or
            (to == .ambigiuous_int and self_type.isInt()) or
            (to == .ambigiuous_float and self_type.isFloat()) or
            (self_type.isInt() and to.isInt() and
            self_type.maxInt() <= to.maxInt() and self_type.minInt() >= to.minInt() and
            self_type.canBeNegative() == to.canBeNegative()) or
            (self_type.isFloat() and to.isFloat() and
            self_type.maxFloat() <= to.maxFloat()) or
            self_type.eql(to);
    }

    fn canBeRepresented(self: Value, as: Type) bool {
        return (self == .int and self.int >= as.minInt() and
            self == .int and self.int <= as.maxInt()) or
            (self == .float and self.float >= -as.maxFloat() and
            self == .float and self.float <= as.maxFloat()) or
            (self != .int and self != .float);
    }

    fn getType(self: Value) Type {
        return switch (self) {
            .int => .ambigiuous_int,
            .float => .ambigiuous_float,
            .boolean => .bool,
            .string => Type{
                .pointer = .{
                    .size = .many,
                    .is_const = true,
                    .child_type = &.{ .int = .{ .signedness = .unsigned, .bits = 8 } },
                },
            },
            inline else => |other| other.type,
        };
    }

    pub fn format(self: Value, _: anytype, _: anytype, writer: anytype) !void {
        switch (self) {
            .int => |int| try writer.print("{}", .{int}),
            .typed_int => |typed_int| try writer.print("{}", .{typed_int.value}),
            .float => |float| try writer.print("{d}", .{float}),
            .boolean => |boolean| try writer.print("{}", .{boolean}),
            .string => |string| try writer.print("{s}", .{string}),
            .runtime => |runtime| try writer.print("<runtime value '{}'>", .{runtime.type}),
        }
    }
};

const Variable = struct {
    symbol: Symbol,
    maybe_value: ?Value = null,
    is_const: bool = false,
    is_comptime: bool = false,
    is_type_alias: bool = false,
};

fn checkRepresentability(self: *Sema, source_value: Value, destination_type: Type, source_loc: SourceLoc) Error!void {
    const source_type = source_value.getType();

    if (!source_value.canImplicitCast(destination_type)) {
        var error_message_buf: std.ArrayListUnmanaged(u8) = .{};

        try error_message_buf.writer(self.allocator).print("'{}' cannot be implicitly casted to '{}'", .{ source_type, destination_type });

        self.error_info = .{ .message = error_message_buf.items, .source_loc = source_loc };

        return error.MismatchedTypes;
    }

    if (!source_value.canBeRepresented(destination_type)) {
        var error_message_buf: std.ArrayListUnmanaged(u8) = .{};

        try error_message_buf.writer(self.allocator).print("'{}' cannot represent value '{}'", .{ destination_type, source_value });

        self.error_info = .{ .message = error_message_buf.items, .source_loc = source_loc };

        return error.TypeCannotRepresentValue;
    }
}

fn checkIntOrFloat(self: *Sema, provided_type: Type, source_loc: SourceLoc) Error!void {
    if (!provided_type.isIntOrFloat()) {
        var error_message_buf: std.ArrayListUnmanaged(u8) = .{};

        try error_message_buf.writer(self.allocator).print("'{}' is provided while expected an integer or float", .{provided_type});
        self.error_info = .{ .message = error_message_buf.items, .source_loc = source_loc };

        return error.MismatchedTypes;
    }
}

fn checkIntOrFloatOrPointer(self: *Sema, provided_type: Type, source_loc: SourceLoc) Error!void {
    if (!provided_type.isIntOrFloatOrPointer()) {
        var error_message_buf: std.ArrayListUnmanaged(u8) = .{};

        try error_message_buf.writer(self.allocator).print("'{}' is provided while expected an integer or float or pointer", .{provided_type});

        self.error_info = .{ .message = error_message_buf.items, .source_loc = source_loc };

        return error.MismatchedTypes;
    }
}

fn checkInt(self: *Sema, provided_type: Type, source_loc: SourceLoc) Error!void {
    if (!provided_type.isInt()) {
        var error_message_buf: std.ArrayListUnmanaged(u8) = .{};

        try error_message_buf.writer(self.allocator).print("'{}' is provided while expected an integer", .{provided_type});

        self.error_info = .{ .message = error_message_buf.items, .source_loc = source_loc };

        return error.MismatchedTypes;
    }
}

fn checkIntType(self: *Sema, provided_type: Type, source_loc: SourceLoc) Error!void {
    if (!provided_type.isInt()) {
        var error_message_buf: std.ArrayListUnmanaged(u8) = .{};

        try error_message_buf.writer(self.allocator).print("'{}' is provided while expected an integer type", .{provided_type});

        self.error_info = .{ .message = error_message_buf.items, .source_loc = source_loc };

        return error.MismatchedTypes;
    }
}

fn checkCanBeCompared(self: *Sema, provided_type: Type, source_loc: SourceLoc) Error!void {
    if (provided_type.getPointer()) |pointer| {
        if (pointer.size == .many) {
            var error_message_buf: std.ArrayListUnmanaged(u8) = .{};

            try error_message_buf.writer(self.allocator).print("'{}' cannot be compared", .{provided_type});

            self.error_info = .{ .message = error_message_buf.items, .source_loc = source_loc };

            return error.MismatchedTypes;
        }
    }
}

fn checkStructOrStructPointer(self: *Sema, provided_type: Type, source_loc: SourceLoc) Error!void {
    if (provided_type == .@"struct") return;
    if (provided_type.getPointer()) |pointer| if (pointer.child_type.* == .@"struct") return;

    var error_message_buf: std.ArrayListUnmanaged(u8) = .{};

    try error_message_buf.writer(self.allocator).print("'{}' is not a struct nor a pointer to a struct", .{provided_type});

    self.error_info = .{ .message = error_message_buf.items, .source_loc = source_loc };

    return error.MismatchedTypes;
}

fn checkTypeCircularDependency(self: *Sema, type_name: Name, provided_subtype: Sir.SubType) Error!void {
    switch (provided_subtype) {
        .name => |referenced_name| {
            if (std.mem.eql(u8, type_name.buffer, referenced_name.buffer)) {
                return self.reportCircularDependency(referenced_name);
            }
        },

        .@"struct" => |referenced_struct| {
            for (referenced_struct.subsymbols) |subsymbol| {
                try self.checkTypeCircularDependency(type_name, subsymbol.subtype);
            }
        },

        else => {},
    }
}

fn reportIncompatibleTypes(self: *Sema, lhs: Type, rhs: Type, source_loc: SourceLoc) Error!void {
    var error_message_buf: std.ArrayListUnmanaged(u8) = .{};

    try error_message_buf.writer(self.allocator).print("'{}' is not compatible with '{}'", .{ lhs, rhs });

    self.error_info = .{ .message = error_message_buf.items, .source_loc = source_loc };

    return error.MismatchedTypes;
}

fn reportNotDeclared(self: *Sema, name: Name) Error!void {
    var error_message_buf: std.ArrayListUnmanaged(u8) = .{};

    try error_message_buf.writer(self.allocator).print("'{s}' is not declared", .{name.buffer});

    self.error_info = .{ .message = error_message_buf.items, .source_loc = name.source_loc };

    return error.Undeclared;
}

fn reportRedeclaration(self: *Sema, name: Name) Error!void {
    var error_message_buf: std.ArrayListUnmanaged(u8) = .{};

    try error_message_buf.writer(self.allocator).print("redeclaration of '{s}'", .{name.buffer});

    self.error_info = .{ .message = error_message_buf.items, .source_loc = name.source_loc };

    return error.Redeclared;
}

fn reportTypeNotDeclared(self: *Sema, name: Name) Error!void {
    var error_message_buf: std.ArrayListUnmanaged(u8) = .{};

    try error_message_buf.writer(self.allocator).print("type '{s}' is not declared", .{name.buffer});

    self.error_info = .{ .message = error_message_buf.items, .source_loc = name.source_loc };

    return error.Undeclared;
}

fn reportTypeNotExpression(self: *Sema, name: Name) Error!void {
    var error_message_buf: std.ArrayListUnmanaged(u8) = .{};

    try error_message_buf.writer(self.allocator).print("'{s}' is a type not an expression", .{name.buffer});

    self.error_info = .{ .message = error_message_buf.items, .source_loc = name.source_loc };

    return error.Undeclared;
}

fn reportNotPointer(self: *Sema, provided_type: Type, source_loc: SourceLoc) Error!void {
    var error_message_buf: std.ArrayListUnmanaged(u8) = .{};

    try error_message_buf.writer(self.allocator).print("'{}' is not a pointer", .{provided_type});

    self.error_info = .{ .message = error_message_buf.items, .source_loc = source_loc };

    return error.MismatchedTypes;
}

fn reportNotIndexable(self: *Sema, provided_type: Type, source_loc: SourceLoc) Error!void {
    var error_message_buf: std.ArrayListUnmanaged(u8) = .{};

    try error_message_buf.writer(self.allocator).print("'{}' does not support indexing", .{provided_type});

    self.error_info = .{ .message = error_message_buf.items, .source_loc = source_loc };

    return error.UnexpectedType;
}

fn reportCircularDependency(self: *Sema, name: Name) Error!void {
    var error_message_buf: std.ArrayListUnmanaged(u8) = .{};

    try error_message_buf.writer(self.allocator).print("'{s}' is circularly dependent on itself", .{name.buffer});

    self.error_info = .{ .message = error_message_buf.items, .source_loc = name.source_loc };

    return error.CircularDependency;
}

pub fn init(allocator: std.mem.Allocator, env: Compilation.Environment, sir: Sir) std.mem.Allocator.Error!Sema {
    var air: Air = .{};

    // Air instructions are always less than or equal to the Sir instructions length
    try air.instructions.ensureTotalCapacity(allocator, sir.instructions.items.len);

    return Sema{
        .allocator = allocator,
        .env = env,
        .sir = sir,
        .air = air,
    };
}

pub fn deinit(self: *Sema) void {
    self.stack.deinit(self.allocator);
    self.scope_stack.deinit(self.allocator);
}

fn putBuiltinConstants(self: *Sema) std.mem.Allocator.Error!void {
    try self.scope.ensureTotalCapacity(self.allocator, 65);

    self.scope.putAssumeCapacity("true", .{
        .is_const = true,
        .is_comptime = true,
        .symbol = undefined,
        .maybe_value = .{ .boolean = true },
    });

    self.scope.putAssumeCapacity("false", .{
        .is_const = true,
        .is_comptime = true,
        .symbol = undefined,
        .maybe_value = .{ .boolean = false },
    });

    self.scope.putAssumeCapacity("void", .{
        .symbol = .{
            .name = undefined,
            .type = .void,
            .linkage = .global,
        },
        .is_type_alias = true,
    });

    self.scope.putAssumeCapacity("bool", .{
        .symbol = .{
            .name = undefined,
            .type = .bool,
            .linkage = .global,
        },
        .is_type_alias = true,
    });

    self.scope.putAssumeCapacity("usize", .{
        .symbol = .{
            .name = undefined,
            .type = .{ .int = .{ .signedness = .unsigned, .bits = self.env.target.ptrBitWidth() } },
            .linkage = .global,
        },
        .is_type_alias = true,
    });
    self.scope.putAssumeCapacity("isize", .{
        .symbol = .{
            .name = undefined,
            .type = .{ .int = .{ .signedness = .signed, .bits = self.env.target.ptrBitWidth() } },
            .linkage = .global,
        },
        .is_type_alias = true,
    });

    self.scope.putAssumeCapacity("c_char", .{
        .symbol = .{
            .name = undefined,
            .type = .{ .int = .{ .signedness = .signed, .bits = self.env.target.cTypeBitSize(.char) } },
            .linkage = .global,
        },
        .is_type_alias = true,
    });
    self.scope.putAssumeCapacity("c_short", .{
        .symbol = .{
            .name = undefined,
            .type = .{ .int = .{ .signedness = .signed, .bits = self.env.target.cTypeBitSize(.short) } },
            .linkage = .global,
        },
        .is_type_alias = true,
    });
    self.scope.putAssumeCapacity("c_ushort", .{
        .symbol = .{
            .name = undefined,
            .type = .{ .int = .{ .signedness = .unsigned, .bits = self.env.target.cTypeBitSize(.ushort) } },
            .linkage = .global,
        },
        .is_type_alias = true,
    });
    self.scope.putAssumeCapacity("c_int", .{
        .symbol = .{
            .name = undefined,
            .type = .{ .int = .{ .signedness = .signed, .bits = self.env.target.cTypeBitSize(.int) } },
            .linkage = .global,
        },
        .is_type_alias = true,
    });
    self.scope.putAssumeCapacity("c_uint", .{
        .symbol = .{
            .name = undefined,
            .type = .{ .int = .{ .signedness = .unsigned, .bits = self.env.target.cTypeBitSize(.uint) } },
            .linkage = .global,
        },
        .is_type_alias = true,
    });
    self.scope.putAssumeCapacity("c_long", .{
        .symbol = .{
            .name = undefined,
            .type = .{ .int = .{ .signedness = .signed, .bits = self.env.target.cTypeBitSize(.long) } },
            .linkage = .global,
        },
        .is_type_alias = true,
    });
    self.scope.putAssumeCapacity("c_ulong", .{
        .symbol = .{
            .name = undefined,
            .type = .{ .int = .{ .signedness = .unsigned, .bits = self.env.target.cTypeBitSize(.ulong) } },
            .linkage = .global,
        },
        .is_type_alias = true,
    });
    self.scope.putAssumeCapacity("c_longlong", .{
        .symbol = .{
            .name = undefined,
            .type = .{ .int = .{ .signedness = .signed, .bits = self.env.target.cTypeBitSize(.longlong) } },
            .linkage = .global,
        },
        .is_type_alias = true,
    });
    self.scope.putAssumeCapacity("c_ulonglong", .{
        .symbol = .{
            .name = undefined,
            .type = .{ .int = .{ .signedness = .unsigned, .bits = self.env.target.cTypeBitSize(.ulonglong) } },
            .linkage = .global,
        },
        .is_type_alias = true,
    });

    self.scope.putAssumeCapacity("c_float", .{
        .symbol = .{
            .name = undefined,
            .type = .{ .float = .{ .bits = self.env.target.cTypeBitSize(.float) } },
            .linkage = .global,
        },
        .is_type_alias = true,
    });
    self.scope.putAssumeCapacity("c_double", .{
        .symbol = .{
            .name = undefined,
            .type = .{ .float = .{ .bits = self.env.target.cTypeBitSize(.double) } },
            .linkage = .global,
        },
        .is_type_alias = true,
    });

    // TODO: Type `c_longdouble` requires `f80` and `f128` to be supported.

    self.scope.putAssumeCapacity("u8", .{
        .symbol = .{
            .name = undefined,
            .type = .{ .int = .{ .signedness = .unsigned, .bits = 8 } },
            .linkage = .global,
        },
        .is_type_alias = true,
    });
    self.scope.putAssumeCapacity("u16", .{
        .symbol = .{
            .name = undefined,
            .type = .{ .int = .{ .signedness = .unsigned, .bits = 16 } },
            .linkage = .global,
        },
        .is_type_alias = true,
    });
    self.scope.putAssumeCapacity("u32", .{
        .symbol = .{
            .name = undefined,
            .type = .{ .int = .{ .signedness = .unsigned, .bits = 32 } },
            .linkage = .global,
        },
        .is_type_alias = true,
    });
    self.scope.putAssumeCapacity("u64", .{
        .symbol = .{
            .name = undefined,
            .type = .{ .int = .{ .signedness = .unsigned, .bits = 64 } },
            .linkage = .global,
        },
        .is_type_alias = true,
    });
    self.scope.putAssumeCapacity("i8", .{
        .symbol = .{
            .name = undefined,
            .type = .{ .int = .{ .signedness = .signed, .bits = 8 } },
            .linkage = .global,
        },
        .is_type_alias = true,
    });
    self.scope.putAssumeCapacity("i16", .{
        .symbol = .{
            .name = undefined,
            .type = .{ .int = .{ .signedness = .signed, .bits = 16 } },
            .linkage = .global,
        },
        .is_type_alias = true,
    });
    self.scope.putAssumeCapacity("i32", .{
        .symbol = .{
            .name = undefined,
            .type = .{ .int = .{ .signedness = .signed, .bits = 32 } },
            .linkage = .global,
        },
        .is_type_alias = true,
    });
    self.scope.putAssumeCapacity("i64", .{
        .symbol = .{
            .name = undefined,
            .type = .{ .int = .{ .signedness = .signed, .bits = 64 } },
            .linkage = .global,
        },
        .is_type_alias = true,
    });

    self.scope.putAssumeCapacity("f32", .{
        .symbol = .{
            .name = undefined,
            .type = .{ .float = .{ .bits = 32 } },
            .linkage = .global,
        },
        .is_type_alias = true,
    });
    self.scope.putAssumeCapacity("f64", .{
        .symbol = .{
            .name = undefined,
            .type = .{ .float = .{ .bits = 64 } },
            .linkage = .global,
        },
        .is_type_alias = true,
    });

    self.scope.putAssumeCapacity("builtin::target::os", .{
        .symbol = .{
            .name = undefined,
            .type = .ambigiuous_int,
            .linkage = .global,
        },

        .maybe_value = .{ .int = @intFromEnum(self.env.target.os.tag) },
        .is_const = true,
        .is_comptime = true,
    });

    self.scope.putAssumeCapacity("builtin::target::arch", .{
        .symbol = .{
            .name = undefined,
            .type = .ambigiuous_int,
            .linkage = .global,
        },

        .maybe_value = .{ .int = @intFromEnum(self.env.target.cpu.arch) },
        .is_const = true,
        .is_comptime = true,
    });
}

pub fn analyze(self: *Sema) Error!void {
    const global_scope = try self.scope_stack.addOne(self.allocator);
    global_scope.* = .{};
    self.scope = global_scope;

    try self.putBuiltinConstants();

    for (self.sir.instructions.items) |instruction| {
        try self.analyzeInstruction(instruction);
    }
}

fn analyzeSubType(self: *Sema, subtype: Sir.SubType) Error!Type {
    switch (subtype) {
        .name => |name| {
            if (self.scope.get(name.buffer)) |variable| {
                if (variable.is_type_alias) {
                    return variable.symbol.type;
                }
            }

            try self.reportTypeNotDeclared(name);

            unreachable;
        },

        .function => |function| {
            const parameter_types = try self.allocator.alloc(Type, function.parameter_subtypes.len);

            for (function.parameter_subtypes, 0..) |parameter_subtype, i| {
                parameter_types[i] = try self.analyzeSubType(parameter_subtype);
            }

            const return_type = try self.analyzeSubType(function.return_subtype.*);

            const return_type_on_heap = try self.allocator.create(Type);
            return_type_on_heap.* = return_type;

            return Type{
                .function = .{
                    .parameter_types = parameter_types,
                    .return_type = return_type_on_heap,
                },
            };
        },

        .pointer => |pointer| {
            if (pointer.child_subtype.* == .name) {
                const child_subtype_name = pointer.child_subtype.name;

                if (self.scope.getPtr(child_subtype_name.buffer)) |child_subtype_variable| {
                    if (child_subtype_variable.is_type_alias) {
                        return Type{
                            .pointer = .{
                                .size = pointer.size,
                                .is_const = pointer.is_const,
                                .child_type = &child_subtype_variable.symbol.type,
                            },
                        };
                    }
                }

                try self.reportTypeNotDeclared(child_subtype_name);

                unreachable;
            } else {
                const child_type = try self.analyzeSubType(pointer.child_subtype.*);

                const child_type_on_heap = try self.allocator.create(Type);
                child_type_on_heap.* = child_type;

                return Type{
                    .pointer = .{
                        .size = pointer.size,
                        .is_const = pointer.is_const,
                        .child_type = child_type_on_heap,
                    },
                };
            }
        },

        .@"struct" => |@"struct"| {
            var fields = try self.allocator.alloc(Type.Struct.Field, @"struct".subsymbols.len);

            for (@"struct".subsymbols, 0..) |subsymbol, i| {
                const symbol = try self.analyzeSubSymbol(subsymbol);

                fields[i] = .{ .name = symbol.name.buffer, .type = symbol.type };
            }

            return Type{ .@"struct" = .{ .fields = fields } };
        },

        .@"enum" => |@"enum"| {
            self.error_info = .{ .message = "enums should be in a type alias as they require a namespace", .source_loc = @"enum".source_loc };

            return error.UnexpectedType;
        },

        .pure => |pure| return pure,
    }
}

fn analyzeSubSymbol(self: *Sema, subsymbol: Sir.SubSymbol) Error!Symbol {
    return Symbol{
        .name = subsymbol.name,
        .type = try self.analyzeSubType(subsymbol.subtype),
        .linkage = subsymbol.linkage,
    };
}

fn analyzeInstruction(self: *Sema, instruction: Sir.Instruction) Error!void {
    switch (instruction) {
        .duplicate => try self.analyzeDuplicate(),
        .reverse => |count| try self.analyzeReverse(count),
        .pop => try self.analyzePop(),

        .string => |string| try self.analyzeString(string),
        .int => |int| try self.analyzeInt(int),
        .float => |float| try self.analyzeFloat(float),

        .negate => |source_loc| try self.analyzeNegate(source_loc),

        .bool_not => |source_loc| try self.analyzeNot(.bool, source_loc),
        .bit_not => |source_loc| try self.analyzeNot(.bit, source_loc),

        .bit_and => |source_loc| try self.analyzeBitwiseArithmetic(.bit_and, source_loc),
        .bit_or => |source_loc| try self.analyzeBitwiseArithmetic(.bit_or, source_loc),
        .bit_xor => |source_loc| try self.analyzeBitwiseArithmetic(.bit_xor, source_loc),

        .write => |source_loc| try self.analyzeWrite(source_loc),
        .read => |source_loc| try self.analyzeRead(source_loc),
        .get_element_ptr => |source_loc| try self.analyzeGetElementPtr(source_loc),
        .get_field_ptr => |name| try self.analyzeGetFieldPtr(name),
        .reference => |source_loc| try self.analyzeReference(source_loc),

        .add => |source_loc| try self.analyzeArithmetic(.add, source_loc),
        .sub => |source_loc| try self.analyzeArithmetic(.sub, source_loc),
        .mul => |source_loc| try self.analyzeArithmetic(.mul, source_loc),
        .div => |source_loc| try self.analyzeArithmetic(.div, source_loc),

        .lt => |source_loc| try self.analyzeComparison(.lt, source_loc),
        .gt => |source_loc| try self.analyzeComparison(.gt, source_loc),
        .eql => |source_loc| try self.analyzeComparison(.eql, source_loc),

        .shl => |source_loc| try self.analyzeBitwiseShift(.left, source_loc),
        .shr => |source_loc| try self.analyzeBitwiseShift(.right, source_loc),

        .cast => |cast| try self.analyzeCast(cast),

        .assembly => |assembly| try self.analyzeAssembly(assembly),

        .call => |call| try self.analyzeCall(call),

        .function => |subsymbol| try self.analyzeFunction(subsymbol),

        .parameters => |subsymbols| try self.analyzeParameters(subsymbols),

        .constant => |subsymbol| try self.analyzeConstant(false, subsymbol),
        .constant_infer => |subsymbol| try self.analyzeConstant(true, subsymbol),

        .variable => |subsymbol| try self.analyzeVariable(false, subsymbol),
        .variable_infer => |subsymbol| try self.analyzeVariable(true, subsymbol),

        .external => |subsymbol| try self.analyzeExternal(subsymbol),

        .type_alias => |subsymbol| try self.analyzeTypeAlias(subsymbol),

        .set => |name| try self.analyzeSet(name),
        .get => |name| try self.analyzeGet(name),

        .block => |block| try self.analyzeBlock(block),
        .br => |br| try self.analyzeBr(br),
        .cond_br => |cond_br| try self.analyzeCondBr(cond_br),

        .start_scope => try self.modifyScope(true),
        .end_scope => try self.modifyScope(false),

        .ret => |source_loc| try self.analyzeReturn(true, source_loc),
        .ret_void => |source_loc| try self.analyzeReturn(false, source_loc),
    }
}

fn analyzeDuplicate(self: *Sema) Error!void {
    try self.stack.append(self.allocator, self.stack.getLast());
    try self.air.instructions.append(self.allocator, .duplicate);
}

fn analyzeReverse(self: *Sema, count: u32) Error!void {
    std.mem.reverse(Value, self.stack.items[self.stack.items.len - count ..]);
    try self.air.instructions.append(self.allocator, .{ .reverse = count });
}

fn analyzePop(self: *Sema) Error!void {
    if (self.stack.popOrNull()) |unused_value| {
        if (unused_value.getType() != .void) {
            try self.air.instructions.append(self.allocator, .pop);
        }
    }
}

fn analyzeString(self: *Sema, string: []const u8) Error!void {
    try self.stack.append(self.allocator, .{ .string = string });

    try self.air.instructions.append(self.allocator, .{ .string = string });
}

fn analyzeInt(self: *Sema, int: i128) Error!void {
    try self.stack.append(self.allocator, .{ .int = int });

    try self.air.instructions.append(self.allocator, .{ .int = int });
}

fn analyzeFloat(self: *Sema, float: f64) Error!void {
    try self.stack.append(self.allocator, .{ .float = float });

    try self.air.instructions.append(self.allocator, .{ .float = float });
}

fn analyzeNegate(self: *Sema, source_loc: SourceLoc) Error!void {
    const rhs = self.stack.pop();

    if (!rhs.getType().canBeNegative()) {
        var error_message_buf: std.ArrayListUnmanaged(u8) = .{};

        try error_message_buf.writer(self.allocator).print("'{}' cannot be negative", .{rhs.getType()});

        self.error_info = .{ .message = error_message_buf.items, .source_loc = source_loc };

        return error.MismatchedTypes;
    }

    switch (rhs) {
        .int => |rhs_int| {
            self.air.instructions.items[self.air.instructions.items.len - 1] = .{ .int = -rhs_int };

            try self.stack.append(self.allocator, .{ .int = -rhs_int });
        },

        .float => |rhs_float| {
            self.air.instructions.items[self.air.instructions.items.len - 1] = .{ .float = -rhs_float };

            try self.stack.append(self.allocator, .{ .float = -rhs_float });
        },

        .runtime => |rhs_runtime| {
            try self.air.instructions.append(self.allocator, .negate);

            try self.stack.append(self.allocator, .{ .runtime = .{ .type = rhs_runtime.type } });
        },

        else => unreachable,
    }
}

const NotOperation = enum {
    bool,
    bit,
};

fn analyzeNot(self: *Sema, comptime operand: NotOperation, source_loc: SourceLoc) Error!void {
    const rhs = self.stack.pop();
    const rhs_type = rhs.getType();

    if (operand == .bool) {
        try self.checkRepresentability(rhs, .bool, source_loc);
    } else if (operand == .bit) {
        try self.checkInt(rhs_type, source_loc);
    }

    switch (rhs) {
        .int => |rhs_int| {
            self.air.instructions.items[self.air.instructions.items.len - 1] = .{ .int = ~rhs_int };

            try self.stack.append(self.allocator, .{ .int = ~rhs_int });
        },

        .boolean => |rhs_boolean| {
            self.air.instructions.items[self.air.instructions.items.len - 1] = .{ .boolean = !rhs_boolean };

            try self.stack.append(self.allocator, .{ .boolean = !rhs_boolean });
        },

        .runtime => |rhs_runtime| {
            try self.air.instructions.append(self.allocator, if (operand == .bool) .bool_not else .bit_not);

            try self.stack.append(self.allocator, .{ .runtime = .{ .type = rhs_runtime.type } });
        },

        else => unreachable,
    }
}

const BitwiseArithmeticOperation = enum {
    bit_and,
    bit_or,
    bit_xor,
};

fn analyzeBitwiseArithmetic(self: *Sema, comptime operation: BitwiseArithmeticOperation, source_loc: SourceLoc) Error!void {
    const rhs = self.stack.pop();
    const lhs = self.stack.pop();

    const lhs_type = lhs.getType();
    const rhs_type = rhs.getType();

    try self.checkInt(lhs_type, source_loc);
    try self.checkInt(rhs_type, source_loc);

    if (!lhs_type.eql(rhs_type) and !lhs_type.isAmbigiuous() and !rhs_type.isAmbigiuous()) {
        return self.reportIncompatibleTypes(lhs_type, rhs_type, source_loc);
    }

    switch (operation) {
        .bit_and => try self.air.instructions.append(self.allocator, .bit_and),
        .bit_or => try self.air.instructions.append(self.allocator, .bit_or),
        .bit_xor => try self.air.instructions.append(self.allocator, .bit_xor),
    }

    switch (lhs) {
        .int => |lhs_int| switch (rhs) {
            .int => |rhs_int| {
                const result = switch (operation) {
                    .bit_and => lhs_int & rhs_int,
                    .bit_or => lhs_int | rhs_int,
                    .bit_xor => lhs_int ^ rhs_int,
                };

                try self.stack.append(self.allocator, .{ .int = result });

                _ = self.air.instructions.pop();

                self.air.instructions.items[self.air.instructions.items.len - 1] = .{ .int = result };

                return;
            },

            else => {},
        },

        else => {},
    }

    if (!lhs_type.isAmbigiuous()) {
        // Check if we can represent the rhs ambigiuous value as lhs type (e.g. x + 4)
        if (rhs_type.isAmbigiuous()) {
            try self.checkRepresentability(rhs, lhs_type, source_loc);
        }

        try self.stack.append(self.allocator, .{ .runtime = .{ .type = lhs_type } });
    } else {
        // Check if we can represent the lhs ambigiuous value as rhs type (e.g. 4 + x)
        if (!rhs_type.isAmbigiuous()) {
            try self.checkRepresentability(lhs, rhs_type, source_loc);
        }

        try self.stack.append(self.allocator, .{ .runtime = .{ .type = rhs_type } });
    }
}

fn analyzeWrite(self: *Sema, source_loc: SourceLoc) Error!void {
    // Intentially swapped the order of operands so it can work with duplicate (i.e value first then duplicate then pointer)
    const lhs = self.stack.pop();
    const lhs_type = lhs.getType();

    const lhs_pointer = lhs_type.getPointer() orelse return self.reportNotPointer(lhs_type, source_loc);

    const rhs = self.stack.pop();

    if (lhs_pointer.is_const) {
        self.error_info = .{ .message = "cannot mutate data pointed by this pointer, it points to read-only data", .source_loc = source_loc };

        return error.UnexpectedMutation;
    }

    try self.checkRepresentability(rhs, lhs_pointer.child_type.*, source_loc);

    try self.air.instructions.append(self.allocator, .write);
}

fn analyzeRead(self: *Sema, source_loc: SourceLoc) Error!void {
    const rhs = self.stack.pop();
    const rhs_type = rhs.getType();

    const rhs_pointer = rhs_type.getPointer() orelse return self.reportNotPointer(rhs_type, source_loc);

    if (rhs_pointer.child_type.* == .function) {
        self.error_info = .{ .message = "cannot read from a function pointer, it can only be called", .source_loc = source_loc };

        return error.UnexpectedFunctionPointer;
    }

    const result_type = rhs_pointer.child_type.*;

    try self.air.instructions.append(self.allocator, .{ .read = result_type });

    try self.stack.append(self.allocator, .{ .runtime = .{ .type = result_type } });
}

fn analyzeGetElementPtr(self: *Sema, source_loc: SourceLoc) Error!void {
    const rhs = self.stack.pop();

    const lhs = self.stack.pop();
    const lhs_type = lhs.getType();

    var lhs_pointer = lhs_type.getPointer() orelse return self.reportNotIndexable(lhs_type, source_loc);

    if (lhs_pointer.size != .many) return try self.reportNotIndexable(lhs_type, source_loc);

    const usize_type: Type = .{ .int = .{ .signedness = .unsigned, .bits = self.env.target.ptrBitWidth() } };

    try self.checkRepresentability(rhs, usize_type, source_loc);

    try self.air.instructions.append(self.allocator, .{ .get_element_ptr = lhs_pointer.child_type.* });

    lhs_pointer.size = .one;

    try self.stack.append(self.allocator, .{ .runtime = .{ .type = .{ .pointer = lhs_pointer } } });
}

fn analyzeGetFieldPtr(self: *Sema, name: Name) Error!void {
    const rhs_type = self.stack.getLast().getType();

    try self.checkStructOrStructPointer(rhs_type, name.source_loc);

    if (rhs_type != .pointer) try self.analyzeReference(name.source_loc);

    const rhs = self.stack.pop();

    const rhs_struct = if (rhs_type.getPointer()) |pointer| pointer.child_type.@"struct" else rhs_type.@"struct";

    for (rhs_struct.fields, 0..) |field, i| {
        if (std.mem.eql(u8, field.name, name.buffer)) {
            try self.air.instructions.append(
                self.allocator,
                .{
                    .get_field_ptr = .{
                        .struct_type = .{ .@"struct" = rhs_struct },
                        .index = @intCast(i),
                    },
                },
            );

            const child_type_on_heap = try self.allocator.create(Type);
            child_type_on_heap.* = field.type;

            return self.stack.append(
                self.allocator,
                .{
                    .runtime = .{
                        .type = .{
                            .pointer = .{
                                .size = .one,
                                .is_const = false,
                                .child_type = child_type_on_heap,
                            },
                        },
                    },
                },
            );
        }
    }

    var error_message_buf: std.ArrayListUnmanaged(u8) = .{};

    try error_message_buf.writer(self.allocator).print("'{s}' is not a field in '{}'", .{ name.buffer, rhs.getType() });

    self.error_info = .{ .message = error_message_buf.items, .source_loc = name.source_loc };

    return error.Undeclared;
}

fn analyzeReference(self: *Sema, source_loc: SourceLoc) Error!void {
    const rhs = self.stack.pop();

    if (!(rhs == .runtime and rhs.runtime.data == .name)) {
        var error_message_buf: std.ArrayListUnmanaged(u8) = .{};

        try error_message_buf.writer(self.allocator).print("'{}' value cannot be referenced", .{rhs.getType()});

        self.error_info = .{ .message = error_message_buf.items, .source_loc = source_loc };

        return error.MismatchedTypes;
    }

    const rhs_runtime_name = rhs.runtime.data.name;
    const rhs_runtime_type = rhs.runtime.type;

    const rhs_variable = self.scope.get(rhs_runtime_name.buffer).?;

    const child_on_heap = try self.allocator.create(Type);
    child_on_heap.* = rhs_runtime_type;

    self.air.instructions.items[self.air.instructions.items.len - 1] = .{ .get_ptr = rhs_runtime_name.buffer };

    try self.stack.append(self.allocator, .{
        .runtime = .{
            .type = .{
                .pointer = .{
                    .size = .one,
                    .is_const = rhs_variable.is_const,
                    .child_type = child_on_heap,
                },
            },
        },
    });
}

const ArithmeticOperation = enum {
    add,
    sub,
    mul,
    div,
};

fn analyzeArithmetic(self: *Sema, comptime operation: ArithmeticOperation, source_loc: SourceLoc) Error!void {
    const rhs = self.stack.pop();
    const lhs = self.stack.pop();

    const lhs_type = lhs.getType();
    const rhs_type = rhs.getType();

    if (operation == .add or operation == .sub) {
        try self.checkIntOrFloatOrPointer(lhs_type, source_loc);
        try self.checkIntOrFloatOrPointer(rhs_type, source_loc);

        const usize_type: Type = .{ .int = .{ .signedness = .unsigned, .bits = self.env.target.ptrBitWidth() } };

        if (lhs_type == .pointer) {
            try self.checkRepresentability(rhs, usize_type, source_loc);
        } else if (rhs_type == .pointer) {
            try self.checkRepresentability(lhs, usize_type, source_loc);
        } else if ((lhs_type.isInt() != rhs_type.isInt()) or
            (!lhs_type.eql(rhs_type) and !lhs_type.isAmbigiuous() and !rhs_type.isAmbigiuous()))
        {
            return self.reportIncompatibleTypes(lhs_type, rhs_type, source_loc);
        }
    } else {
        try self.checkIntOrFloat(lhs_type, source_loc);
        try self.checkIntOrFloat(rhs_type, source_loc);

        if ((lhs_type.isInt() != rhs_type.isInt()) or
            (!lhs_type.eql(rhs_type) and !lhs_type.isAmbigiuous() and !rhs_type.isAmbigiuous()))
        {
            return self.reportIncompatibleTypes(lhs_type, rhs_type, source_loc);
        }
    }

    switch (lhs) {
        .int => |lhs_int| switch (rhs) {
            .int => |rhs_int| {
                const result = switch (operation) {
                    .add => lhs_int + rhs_int,
                    .sub => lhs_int - rhs_int,
                    .mul => lhs_int * rhs_int,
                    .div => @divFloor(lhs_int, rhs_int),
                };

                try self.stack.append(self.allocator, .{ .int = result });

                _ = self.air.instructions.pop();

                self.air.instructions.items[self.air.instructions.items.len - 1] = .{ .int = result };

                return;
            },

            else => {},
        },

        .float => |lhs_float| switch (rhs) {
            .float => |rhs_float| {
                const result = switch (operation) {
                    .add => lhs_float + rhs_float,
                    .sub => lhs_float - rhs_float,
                    .mul => lhs_float * rhs_float,
                    .div => lhs_float / rhs_float,
                };

                try self.stack.append(self.allocator, .{ .float = result });

                _ = self.air.instructions.pop();

                self.air.instructions.items[self.air.instructions.items.len - 1] = .{ .float = result };

                return;
            },

            else => {},
        },

        else => {},
    }

    if (lhs_type == .pointer) {
        try self.stack.append(self.allocator, .{ .runtime = .{ .type = lhs_type } });
    } else if (rhs_type == .pointer) {
        try self.stack.append(self.allocator, .{ .runtime = .{ .type = rhs_type } });
    } else if (!lhs_type.isAmbigiuous()) {
        // Check if we can represent the rhs ambigiuous value as lhs type (e.g. x + 4)
        if (rhs_type.isAmbigiuous()) {
            try self.checkRepresentability(rhs, lhs_type, source_loc);
        }

        try self.stack.append(self.allocator, .{ .runtime = .{ .type = lhs_type } });
    } else {
        // Check if we can represent the lhs ambigiuous value as rhs type (e.g. 4 + x)
        if (!rhs_type.isAmbigiuous()) {
            try self.checkRepresentability(lhs, rhs_type, source_loc);
        }

        try self.stack.append(self.allocator, .{ .runtime = .{ .type = rhs_type } });
    }

    switch (operation) {
        .add => try self.air.instructions.append(self.allocator, .add),
        .sub => try self.air.instructions.append(self.allocator, .sub),
        .mul => try self.air.instructions.append(self.allocator, .mul),
        .div => try self.air.instructions.append(
            self.allocator,
            if (lhs_type.isInt() and lhs_type.canBeNegative() and rhs_type.canBeNegative())
                .sdiv
            else if (lhs_type.isFloat())
                .fdiv
            else
                .udiv,
        ),
    }
}

const ComparisonOperation = enum {
    lt,
    gt,
    eql,
};

fn analyzeComparison(self: *Sema, comptime operation: ComparisonOperation, source_loc: SourceLoc) Error!void {
    const rhs = self.stack.pop();
    const lhs = self.stack.pop();

    const lhs_type = lhs.getType();
    const rhs_type = rhs.getType();

    if (operation == .lt or operation == .gt) {
        try self.checkIntOrFloat(lhs_type, source_loc);
        try self.checkIntOrFloat(rhs_type, source_loc);
    }

    try self.checkCanBeCompared(lhs_type, source_loc);
    try self.checkCanBeCompared(rhs_type, source_loc);

    try self.checkRepresentability(lhs, rhs_type, source_loc);
    try self.checkRepresentability(rhs, lhs_type, source_loc);

    switch (lhs) {
        .int => |lhs_int| switch (rhs) {
            .int => |rhs_int| {
                const result = switch (operation) {
                    .lt => lhs_int < rhs_int,
                    .gt => lhs_int > rhs_int,
                    .eql => lhs_int == rhs_int,
                };

                _ = self.air.instructions.pop();

                self.air.instructions.items[self.air.instructions.items.len - 1] = .{ .boolean = result };

                try self.stack.append(self.allocator, .{ .boolean = result });

                return;
            },

            else => {},
        },

        .float => |lhs_float| switch (rhs) {
            .float => |rhs_float| {
                const result = switch (operation) {
                    .lt => lhs_float < rhs_float,
                    .gt => lhs_float > rhs_float,
                    .eql => lhs_float == rhs_float,
                };

                _ = self.air.instructions.pop();

                self.air.instructions.items[self.air.instructions.items.len - 1] = .{ .boolean = result };

                try self.stack.append(self.allocator, .{ .boolean = result });

                return;
            },

            else => {},
        },

        .boolean => |lhs_boolean| switch (rhs) {
            .boolean => |rhs_boolean| {
                const result = switch (operation) {
                    .eql => lhs_boolean == rhs_boolean,

                    else => unreachable,
                };

                _ = self.air.instructions.pop();

                self.air.instructions.items[self.air.instructions.items.len - 1] = .{ .boolean = result };

                try self.stack.append(self.allocator, .{ .boolean = result });

                return;
            },

            else => {},
        },

        else => {},
    }

    switch (operation) {
        .lt => try self.air.instructions.append(self.allocator, if (lhs.getType().isInt())
            .{ .icmp = if (lhs.getType().canBeNegative() and rhs.getType().canBeNegative()) .slt else .ult }
        else
            .{ .fcmp = .lt }),

        .gt => try self.air.instructions.append(self.allocator, if (lhs.getType().isInt())
            .{ .icmp = if (lhs.getType().canBeNegative() and rhs.getType().canBeNegative()) .sgt else .ugt }
        else
            .{ .fcmp = .gt }),

        .eql => try self.air.instructions.append(self.allocator, if (lhs.getType().isInt() or lhs.getType() == .bool)
            .{ .icmp = .eql }
        else
            .{ .fcmp = .eql }),
    }

    try self.stack.append(self.allocator, .{ .runtime = .{ .type = .bool } });
}

const BitwiseShiftDirection = enum {
    left,
    right,
};

fn analyzeBitwiseShift(self: *Sema, comptime direction: BitwiseShiftDirection, source_loc: SourceLoc) Error!void {
    const rhs = self.stack.pop();
    const lhs = self.stack.pop();

    const lhs_type = lhs.getType();

    try self.checkInt(lhs_type, source_loc);
    try self.checkRepresentability(rhs, .{ .int = .{ .signedness = .unsigned, .bits = 8 } }, source_loc);

    if (lhs != .runtime and rhs != .runtime) {
        const lhs_int = lhs.int;
        const rhs_int = rhs.int;

        if (rhs_int > std.math.maxInt(u7)) {
            self.error_info = .{
                .message = "cannot bit shift with a count more than '" ++ std.fmt.comptimePrint("{}", .{std.math.maxInt(u7)}) ++ "'",
                .source_loc = source_loc,
            };

            return error.TypeCannotRepresentValue;
        }

        const result = switch (direction) {
            .left => lhs_int << @intCast(rhs_int),
            .right => lhs_int >> @intCast(rhs_int),
        };

        try self.stack.append(self.allocator, .{ .int = result });

        _ = self.air.instructions.pop();

        self.air.instructions.items[self.air.instructions.items.len - 1] = .{ .int = result };
    } else {
        switch (direction) {
            .left => try self.air.instructions.append(self.allocator, .shl),
            .right => try self.air.instructions.append(self.allocator, .shr),
        }

        try self.stack.append(self.allocator, .{ .runtime = .{ .type = lhs_type } });
    }
}

fn analyzeCast(self: *Sema, cast: Sir.Instruction.Cast) Error!void {
    const from = self.stack.getLast().getType();
    const to = try self.analyzeSubType(cast.to);

    if (from.eql(to)) return;

    const rhs = self.stack.pop();

    if (to == .void) {
        self.error_info = .{ .message = "cannot cast to 'void' as it is not possible to represent a value of this type", .source_loc = cast.source_loc };

        return error.UnexpectedType;
    } else if (to == .function) {
        self.error_info = .{ .message = "cannot cast to a function type as it should be always wrapped in a pointer", .source_loc = cast.source_loc };

        return error.UnexpectedType;
    } else if (to == .@"struct" or from == .@"struct") {
        self.error_info = .{ .message = "cannot cast from or to a struct as it has multiple fields that should be casted individually", .source_loc = cast.source_loc };

        return error.UnexpectedType;
    } else if (to == .pointer and from != .pointer) {
        const usize_type: Type = .{ .int = .{ .signedness = .unsigned, .bits = self.env.target.ptrBitWidth() } };

        try self.checkRepresentability(rhs, usize_type, cast.source_loc);
    } else if (to == .bool) {
        try self.checkInt(from, cast.source_loc);
    } else if (from == .bool) {
        try self.checkInt(to, cast.source_loc);
    } else if (from.isFloat()) {
        try self.checkIntOrFloat(to, cast.source_loc);
    }

    try self.air.instructions.append(self.allocator, .{ .cast = .{ .from = from, .to = to } });

    try self.stack.append(self.allocator, .{ .runtime = .{ .type = to } });
}

fn analyzeAssembly(self: *Sema, assembly: Sir.Instruction.Assembly) Error!void {
    self.stack.shrinkRetainingCapacity(self.stack.items.len - assembly.input_constraints.len);

    if (assembly.output_constraint) |output_constraint| {
        const output_constraint_type = try self.analyzeSubType(output_constraint.subtype);

        try self.air.instructions.append(self.allocator, .{
            .assembly = .{
                .content = assembly.content,
                .input_constraints = assembly.input_constraints,
                .output_constraint = .{
                    .register = output_constraint.register,
                    .type = output_constraint_type,
                },
                .clobbers = assembly.clobbers,
            },
        });

        try self.stack.append(self.allocator, .{ .runtime = .{ .type = output_constraint_type } });
    } else {
        try self.air.instructions.append(self.allocator, .{
            .assembly = .{
                .content = assembly.content,
                .input_constraints = assembly.input_constraints,
                .output_constraint = null,
                .clobbers = assembly.clobbers,
            },
        });

        try self.stack.append(self.allocator, .{ .runtime = .{ .type = .void } });
    }
}

fn analyzeCall(self: *Sema, call: Sir.Instruction.Call) Error!void {
    const callable = self.stack.pop();
    const callable_type = callable.getType();

    var error_message_buf: std.ArrayListUnmanaged(u8) = .{};

    if (callable_type.getFunction()) |function| {
        if (function.parameter_types.len != call.arguments_count) {
            try error_message_buf.writer(self.allocator).print("expected {} argument(s) got {} argument(s)", .{ function.parameter_types.len, call.arguments_count });

            self.error_info = .{ .message = error_message_buf.items, .source_loc = call.source_loc };

            return error.UnexpectedArgumentsCount;
        }

        for (function.parameter_types) |parameter_type| {
            const argument = self.stack.pop();

            try self.checkRepresentability(argument, parameter_type, call.source_loc);
        }

        try self.air.instructions.append(self.allocator, .{ .call = function });

        try self.stack.append(self.allocator, .{ .runtime = .{ .type = function.return_type.*, .data = .none } });
    } else {
        try error_message_buf.writer(self.allocator).print("'{}' is not a callable", .{callable_type});

        self.error_info = .{ .message = error_message_buf.items, .source_loc = call.source_loc };

        return error.MismatchedTypes;
    }
}

fn analyzeFunction(self: *Sema, subsymbol: Sir.SubSymbol) Error!void {
    const symbol = try self.analyzeSubSymbol(subsymbol);
    if (self.scope.get(symbol.name.buffer) != null) return self.reportRedeclaration(symbol.name);

    self.maybe_function = symbol.type;

    try self.air.instructions.append(self.allocator, .{
        .function = .{
            .name = symbol.name.buffer,
            .type = symbol.type,
        },
    });

    try self.scope.put(self.allocator, symbol.name.buffer, .{ .symbol = symbol });
}

fn analyzeParameters(self: *Sema, subsymbols: []const Sir.SubSymbol) Error!void {
    var symbols: std.ArrayListUnmanaged(Symbol) = .{};
    try symbols.ensureTotalCapacity(self.allocator, subsymbols.len);

    for (subsymbols) |subsymbol| {
        const symbol = try self.analyzeSubSymbol(subsymbol);
        if (self.scope.get(symbol.name.buffer) != null) return self.reportRedeclaration(symbol.name);

        symbols.appendAssumeCapacity(symbol);

        try self.scope.put(self.allocator, symbol.name.buffer, .{
            .symbol = symbol,
            .is_const = true,
        });
    }

    try self.air.instructions.append(self.allocator, .{ .parameters = try symbols.toOwnedSlice(self.allocator) });
}

fn analyzeConstant(self: *Sema, infer: bool, subsymbol: Sir.SubSymbol) Error!void {
    const symbol = try self.analyzeSubSymbol(subsymbol);
    if (self.scope.get(symbol.name.buffer) != null) return self.reportRedeclaration(symbol.name);

    var variable: Variable = .{ .is_const = true, .is_comptime = true, .symbol = symbol };

    const value = self.stack.getLast();

    if (infer) {
        variable.symbol.type = value.getType();
    }

    if (variable.symbol.type == .void) {
        self.error_info = .{ .message = "cannot declare a constant with type 'void'", .source_loc = variable.symbol.name.source_loc };

        return error.UnexpectedType;
    }

    if (value == .runtime) {
        self.error_info = .{ .message = "expected the constant initializer to be compile time known", .source_loc = variable.symbol.name.source_loc };

        return error.ExpectedCompiletimeConstant;
    }

    if (variable.symbol.linkage != .global) {
        _ = self.air.instructions.pop();
    }

    try self.scope.put(self.allocator, variable.symbol.name.buffer, variable);
}

fn analyzeVariable(self: *Sema, infer: bool, subsymbol: Sir.SubSymbol) Error!void {
    const symbol = try self.analyzeSubSymbol(subsymbol);
    if (self.scope.get(symbol.name.buffer) != null) return self.reportRedeclaration(symbol.name);

    var variable: Variable = .{ .symbol = symbol };

    if (infer) {
        variable.symbol.type = self.stack.getLast().getType();
    }

    if (variable.symbol.type == .void) {
        self.error_info = .{ .message = "cannot declare a variable with type 'void'", .source_loc = variable.symbol.name.source_loc };

        return error.UnexpectedType;
    }

    if (variable.symbol.type.isAmbigiuous()) {
        self.error_info = .{ .message = "cannot declare a variable with an ambigiuous type", .source_loc = variable.symbol.name.source_loc };

        return error.UnexpectedType;
    }

    try self.scope.put(self.allocator, variable.symbol.name.buffer, variable);

    try self.air.instructions.append(self.allocator, .{ .variable = variable.symbol });
}

fn analyzeExternal(self: *Sema, subsymbol: Sir.SubSymbol) Error!void {
    const symbol = try self.analyzeSubSymbol(subsymbol);
    if (self.scope.get(symbol.name.buffer) != null) return self.reportRedeclaration(symbol.name);

    try self.scope.put(self.allocator, symbol.name.buffer, .{ .symbol = symbol });

    try self.air.instructions.append(self.allocator, .{ .external = symbol });
}

fn analyzeTypeAlias(self: *Sema, subsymbol: Sir.SubSymbol) Error!void {
    if (self.scope.get(subsymbol.name.buffer) != null) return self.reportRedeclaration(subsymbol.name);

    try self.checkTypeCircularDependency(subsymbol.name, subsymbol.subtype);

    if (subsymbol.subtype == .@"enum") {
        const @"enum" = subsymbol.subtype.@"enum";
        const enum_type = try self.analyzeSubType(@"enum".subtype.*);

        try self.checkIntType(enum_type, @"enum".source_loc);

        if (enum_type.int.bits > 64) {
            // TODO: LLVM supports more than 64 bits but we don't currently expose this feature, sorry...
            self.error_info = .{
                .message = "enum is backed by an integer that takes more than 64 bits in memory but it is not currently supported",
                .source_loc = @"enum".source_loc,
            };

            return error.UnexpectedType;
        }

        for (@"enum".fields) |field| {
            const enum_field_value: Value = .{ .int = field.value };

            try self.checkRepresentability(enum_field_value, enum_type, field.name.source_loc);

            const enum_field_typed_value: Value = .{ .typed_int = .{ .type = enum_type, .value = field.value } };

            const enum_field_entry = try std.mem.concat(self.allocator, u8, &.{ subsymbol.name.buffer, "::", field.name.buffer });

            if (self.scope.get(enum_field_entry) != null)
                return self.reportRedeclaration(.{ .buffer = enum_field_entry, .source_loc = field.name.source_loc });

            try self.scope.put(self.allocator, enum_field_entry, .{
                .symbol = undefined,
                .maybe_value = enum_field_typed_value,
                .is_const = true,
                .is_comptime = true,
            });
        }

        const symbol_entry = try self.scope.getOrPut(self.allocator, subsymbol.name.buffer);
        symbol_entry.value_ptr.is_type_alias = true;
        symbol_entry.value_ptr.symbol = .{ .name = subsymbol.name, .type = enum_type, .linkage = subsymbol.linkage };
    } else {
        const symbol_entry = try self.scope.getOrPut(self.allocator, subsymbol.name.buffer);
        symbol_entry.value_ptr.is_type_alias = true;
        symbol_entry.value_ptr.symbol = try self.analyzeSubSymbol(subsymbol);
    }
}

fn analyzeSet(self: *Sema, name: Name) Error!void {
    const variable = self.scope.getPtr(name.buffer) orelse return self.reportNotDeclared(name);
    if (variable.is_type_alias) return self.reportTypeNotExpression(name);

    const value = self.stack.pop();

    if (variable.is_comptime and variable.maybe_value == null) {
        variable.maybe_value = value;
    } else if (variable.is_const) {
        self.error_info = .{ .message = "cannot mutate the value of a constant", .source_loc = name.source_loc };

        return error.UnexpectedMutation;
    } else if (variable.symbol.linkage == .global and value == .runtime and self.maybe_function == null) {
        self.error_info = .{ .message = "expected global variable initializer to be compile time known", .source_loc = name.source_loc };

        return error.ExpectedCompiletimeConstant;
    } else {
        try self.checkRepresentability(value, variable.symbol.type, name.source_loc);

        if (self.maybe_function != null) {
            try self.air.instructions.append(self.allocator, .{ .set = name.buffer });
        }
    }
}

fn analyzeGet(self: *Sema, name: Name) Error!void {
    const variable = self.scope.get(name.buffer) orelse return self.reportNotDeclared(name);
    if (variable.is_type_alias) return self.reportTypeNotExpression(name);

    if (variable.maybe_value) |value| {
        switch (value) {
            .string => |string| try self.air.instructions.append(self.allocator, .{ .string = string }),
            .int => |int| try self.air.instructions.append(self.allocator, .{ .int = int }),
            .typed_int => |typed_int| try self.air.instructions.append(self.allocator, .{ .int = typed_int.value }),
            .float => |float| try self.air.instructions.append(self.allocator, .{ .float = float }),
            .boolean => |boolean| try self.air.instructions.append(self.allocator, .{ .boolean = boolean }),
            .runtime => unreachable,
        }

        try self.stack.append(self.allocator, value);
    } else {
        if (variable.symbol.type.getFunction() != null) {
            try self.air.instructions.append(self.allocator, .{ .get_ptr = name.buffer });
        } else {
            try self.air.instructions.append(self.allocator, .{ .get = name.buffer });
        }

        try self.stack.append(self.allocator, .{ .runtime = .{ .type = variable.symbol.type, .data = .{ .name = variable.symbol.name } } });
    }
}

fn analyzeBlock(self: *Sema, block: Sir.Instruction.Block) Error!void {
    try self.air.instructions.append(self.allocator, .{ .block = .{ .id = block.id } });
}

fn analyzeBr(self: *Sema, br: Sir.Instruction.Br) Error!void {
    try self.air.instructions.append(self.allocator, .{ .br = .{ .id = br.id } });
}

fn analyzeCondBr(self: *Sema, cond_br: Sir.Instruction.CondBr) Error!void {
    const condition = self.stack.pop();

    try self.checkRepresentability(condition, .bool, cond_br.source_loc);

    switch (condition) {
        .boolean => |condition_boolean| {
            _ = self.air.instructions.pop();

            if (condition_boolean == false) {
                try self.air.instructions.append(self.allocator, .{ .br = .{ .id = cond_br.false_id } });
            } else {
                try self.air.instructions.append(self.allocator, .{ .br = .{ .id = cond_br.true_id } });
            }
        },

        else => {
            try self.air.instructions.append(
                self.allocator,
                .{
                    .cond_br = .{
                        .true_id = cond_br.true_id,
                        .false_id = cond_br.false_id,
                    },
                },
            );
        },
    }
}

fn modifyScope(self: *Sema, start: bool) Error!void {
    if (start) {
        const local_scope = try self.scope_stack.addOne(self.allocator);
        local_scope.* = .{ .maybe_parent = self.scope };
        self.scope = local_scope;

        try self.air.instructions.append(self.allocator, .start_scope);
    } else {
        self.scope.clearAndFree(self.allocator);
        self.scope = self.scope.maybe_parent.?;
        _ = self.scope_stack.pop();

        try self.air.instructions.append(self.allocator, .end_scope);
    }
}

fn analyzeReturn(self: *Sema, with_value: bool, source_loc: SourceLoc) Error!void {
    std.debug.assert(self.maybe_function != null);

    const return_type = self.maybe_function.?.pointer.child_type.*.function.return_type.*;

    if (with_value) {
        try self.checkRepresentability(self.stack.pop(), return_type, source_loc);
    } else {
        if (return_type != .void) {
            self.error_info = .{ .message = "function with non void return type returns void", .source_loc = source_loc };

            return error.ExpectedExplicitReturn;
        }
    }

    try self.air.instructions.append(self.allocator, if (with_value) .ret else .ret_void);
}
