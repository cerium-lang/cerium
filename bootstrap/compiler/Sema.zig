//! Semantic Analyzer.
//!
//! An analyzer that lowers down `Sir` to `Air` while checking if the instructions and the types are valid.

const std = @import("std");
const root = @import("root");

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

module: []const u8,

file: Compilation.File,

compilation: *const Compilation,

airs: std.ArrayListUnmanaged(Air) = .{},
air: *Air = undefined,

maybe_function_type: ?Type = null,

stack: std.ArrayListUnmanaged(Value) = .{},

scope: *Scope(Variable),
scopes: std.ArrayListUnmanaged(Scope(Variable)),

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
    UnexpectedValue,
    CircularDependency,
    UnexpectedType,
    MismatchedTypes,
    UnnamedModule,
    MultipleModules,
    ImportFailed,
    Undeclared,
    Redeclared,
} || std.mem.Allocator.Error;

const Value = union(enum) {
    string: []const u8,
    int: i128,
    float: f64,
    boolean: bool,
    runtime: Type,

    fn getType(self: Value) Type {
        return switch (self) {
            .int => |int| Type.intFittingRange(int, int),
            .float => |float| Type.floatFittingRange(float, float),
            .boolean => .bool,
            .string => |string| Type.string(string.len + 1), // +1 for the null termination
            .runtime => |runtime| runtime,
        };
    }

    pub fn format(self: Value, _: anytype, _: anytype, writer: anytype) !void {
        switch (self) {
            .int => |int| try writer.print("{}", .{int}),
            .float => |float| try writer.print("{d}", .{float}),
            .boolean => |boolean| try writer.print("{}", .{boolean}),
            .string => |string| try writer.print("\"{s}\"", .{string}),
            .runtime => |runtime| try writer.print("<runtime value '{}'>", .{runtime}),
        }
    }
};

const Variable = struct {
    type: Type,
    linkage: Symbol.Linkage,
    maybe_prefixed: ?[]const u8 = null,
    maybe_value: ?Value = null,
    is_const: bool = false,
    is_comptime: bool = false,
    is_type_alias: bool = false,
};

pub fn prefix(allocator: std.mem.Allocator, module: []const u8, symbol: []const u8) std.mem.Allocator.Error![]u8 {
    return std.fmt.allocPrint(allocator, "{s}::{s}", .{ module, symbol });
}

pub fn init(allocator: std.mem.Allocator, file: Compilation.File, compilation: *const Compilation) std.mem.Allocator.Error!Sema {
    var scopes: @FieldType(Sema, "scopes") = .{};

    const global_scope = try scopes.addOne(allocator);
    global_scope.* = .{};

    var sema: Sema = .{
        .allocator = allocator,
        .module = "",
        .file = file,
        .compilation = compilation,
        .scope = global_scope,
        .scopes = scopes,
    };

    try sema.putBuiltinConstants();

    return sema;
}

pub fn deinit(self: *Sema) void {
    self.stack.deinit(self.allocator);
    self.scopes.deinit(self.allocator);
}

fn putBuiltinConstants(self: *Sema) std.mem.Allocator.Error!void {
    try self.scope.ensureTotalCapacity(self.allocator, 256);

    {
        inline for (.{ "void", "bool" }, .{ .void, .bool }) |name, @"type"| {
            self.scope.putAssumeCapacity(name, .{
                .type = @"type",
                .linkage = .global,
                .is_type_alias = true,
            });
        }
    }

    {
        const c_char_bits = self.compilation.env.target.cTypeBitSize(.char);
        const c_short_bits = self.compilation.env.target.cTypeBitSize(.short);
        const c_ushort_bits = self.compilation.env.target.cTypeBitSize(.ushort);
        const c_int_bits = self.compilation.env.target.cTypeBitSize(.int);
        const c_uint_bits = self.compilation.env.target.cTypeBitSize(.uint);
        const c_long_bits = self.compilation.env.target.cTypeBitSize(.long);
        const c_ulong_bits = self.compilation.env.target.cTypeBitSize(.ulong);
        const c_longlong_bits = self.compilation.env.target.cTypeBitSize(.longlong);
        const c_ulonglong_bits = self.compilation.env.target.cTypeBitSize(.ulonglong);
        const ptr_bits = self.compilation.env.target.ptrBitWidth();

        self.scope.putAssumeCapacity("c_char", .{
            .type = .{
                .int = .{
                    .signedness = if (self.compilation.env.target.charSignedness() == .signed) .signed else .unsigned,
                    .bits = c_char_bits,
                },
            },
            .linkage = .global,
            .is_type_alias = true,
        });

        inline for (.{ "c_uchar", "c_ushort", "c_uint", "c_ulong", "c_ulonglong", "usize" }, .{ c_char_bits, c_ushort_bits, c_uint_bits, c_ulong_bits, c_ulonglong_bits, ptr_bits }) |name, bits| {
            self.scope.putAssumeCapacity(name, .{
                .type = .{ .int = .{ .signedness = .unsigned, .bits = @intCast(bits) } },
                .linkage = .global,
                .is_type_alias = true,
            });
        }

        inline for (.{ "c_schar", "c_short", "c_int", "c_long", "c_longlong", "ssize" }, .{ c_char_bits, c_short_bits, c_int_bits, c_long_bits, c_longlong_bits, ptr_bits }) |name, bits| {
            self.scope.putAssumeCapacity(name, .{
                .type = .{ .int = .{ .signedness = .signed, .bits = @intCast(bits) } },
                .linkage = .global,
                .is_type_alias = true,
            });
        }
    }

    // TODO: Find a better way, this is very verbose and doesn't scale well for bigger arbitrary sized integer types
    {
        const unsigned_int_names = [_][]const u8{ "u0", "u1", "u2", "u3", "u4", "u5", "u6", "u7", "u8", "u9", "u10", "u11", "u12", "u13", "u14", "u15", "u16", "u17", "u18", "u19", "u20", "u21", "u22", "u23", "u24", "u25", "u26", "u27", "u28", "u29", "u30", "u31", "u32", "u33", "u34", "u35", "u36", "u37", "u38", "u39", "u40", "u41", "u42", "u43", "u44", "u45", "u46", "u47", "u48", "u49", "u50", "u51", "u52", "u53", "u54", "u55", "u56", "u57", "u58", "u59", "u60", "u61", "u62", "u63", "u64" };

        for (unsigned_int_names, 0..) |name, bits| {
            self.scope.putAssumeCapacity(name, .{
                .type = .{ .int = .{ .signedness = .unsigned, .bits = @intCast(bits) } },
                .linkage = .global,
                .is_type_alias = true,
            });
        }

        const signed_int_names = [_][]const u8{ "s0", "s1", "s2", "s3", "s4", "s5", "s6", "s7", "s8", "s9", "s10", "s11", "s12", "s13", "s14", "s15", "s16", "s17", "s18", "s19", "s20", "s21", "s22", "s23", "s24", "s25", "s26", "s27", "s28", "s29", "s30", "s31", "s32", "s33", "s34", "s35", "s36", "s37", "s38", "s39", "s40", "s41", "s42", "s43", "s44", "s45", "s46", "s47", "s48", "s49", "s50", "s51", "s52", "s53", "s54", "s55", "s56", "s57", "s58", "s59", "s60", "s61", "s62", "s63", "s64" };

        for (signed_int_names, 0..) |name, bits| {
            self.scope.putAssumeCapacity(name, .{
                .type = .{ .int = .{ .signedness = .signed, .bits = @intCast(bits) } },
                .linkage = .global,
                .is_type_alias = true,
            });
        }
    }

    {
        const c_float_bits = self.compilation.env.target.cTypeBitSize(.float);
        const c_double_bits = self.compilation.env.target.cTypeBitSize(.double);
        // TODO: Type `c_longdouble` requires `f80` and `f128` to be supported.

        inline for (.{ "f16", "f32", "f64", "c_float", "c_double" }, .{ 16, 32, 64, c_float_bits, c_double_bits }) |float_type, i| {
            self.scope.putAssumeCapacity(float_type, .{
                .type = .{ .float = .{ .bits = @intCast(i) } },
                .linkage = .global,
                .is_type_alias = true,
            });
        }
    }

    {
        const builtin_target_os = @intFromEnum(self.compilation.env.target.os.tag);
        const builtin_target_arch = @intFromEnum(self.compilation.env.target.cpu.arch);

        inline for (.{ "builtin::target::os", "builtin::target::arch" }, .{ builtin_target_os, builtin_target_arch }) |builtin_name, builtin_value| {
            self.scope.putAssumeCapacity(builtin_name, .{
                .type = Type.intFittingRange(builtin_value, builtin_value),
                .linkage = .global,
                .maybe_value = .{ .int = builtin_value },
                .is_const = true,
                .is_comptime = true,
            });
        }
    }

    {
        inline for (.{ "true", "false" }, .{ true, false }) |boolean_name, boolean_value| {
            self.scope.putAssumeCapacity(boolean_name, .{
                .type = .bool,
                .linkage = .global,
                .maybe_value = .{ .boolean = boolean_value },
                .is_const = true,
                .is_comptime = true,
            });
        }
    }
}

fn import(self: *Sema, file_path: Name) Error!void {
    const import_root = std.mem.eql(u8, file_path.buffer, "root");

    const compilation_file: Compilation.File = if (import_root)
        self.compilation.root_file
    else blk: {
        var in_lib_dir = true;

        const parent_dir_path = std.fs.path.dirname(self.file.path) orelse if (self.file.path[0] == std.fs.path.sep)
            std.fs.path.sep_str
        else
            ".";

        const import_file = self.compilation.env.cerium_lib_dir.openFile(file_path.buffer, .{}) catch nested_blk: {
            const parent_dir = std.fs.cwd().openDir(parent_dir_path, .{}) catch |err| {
                var error_message_buf: std.ArrayListUnmanaged(u8) = .{};

                try error_message_buf.writer(self.allocator).print(
                    "could not open parent directory of root file: {s}",
                    .{root.Cli.errorDescription(err)},
                );

                self.error_info = .{ .message = error_message_buf.items, .source_loc = SourceLoc.find(self.file.buffer, file_path.token_start) };

                return error.ImportFailed;
            };

            in_lib_dir = false;

            break :nested_blk parent_dir.openFile(file_path.buffer, .{}) catch |err| {
                var error_message_buf: std.ArrayListUnmanaged(u8) = .{};

                try error_message_buf.writer(self.allocator).print(
                    "could not open import file: {s}",
                    .{root.Cli.errorDescription(err)},
                );

                self.error_info = .{ .message = error_message_buf.items, .source_loc = SourceLoc.find(self.file.buffer, file_path.token_start) };

                return error.ImportFailed;
            };
        };

        const import_file_buffer = import_file.readToEndAllocOptions(self.allocator, std.math.maxInt(u32), null, @alignOf(u8), 0) catch |err| {
            var error_message_buf: std.ArrayListUnmanaged(u8) = .{};

            try error_message_buf.writer(self.allocator).print(
                "could not read import file: {s}",
                .{root.Cli.errorDescription(err)},
            );

            self.error_info = .{ .message = error_message_buf.items, .source_loc = SourceLoc.find(self.file.buffer, file_path.token_start) };

            return error.ImportFailed;
        };

        const import_file_path = if (in_lib_dir)
            self.compilation.env.cerium_lib_dir.realpathAlloc(self.allocator, file_path.buffer) catch |err| {
                var error_message_buf: std.ArrayListUnmanaged(u8) = .{};

                try error_message_buf.writer(self.allocator).print(
                    "could not find import file path: '{s}'",
                    .{root.Cli.errorDescription(err)},
                );

                self.error_info = .{ .message = error_message_buf.items, .source_loc = SourceLoc.find(self.file.buffer, file_path.token_start) };

                return error.ImportFailed;
            }
        else
            try std.fs.path.resolve(self.allocator, &.{ parent_dir_path, file_path.buffer });

        break :blk .{ .path = import_file_path, .buffer = import_file_buffer };
    };

    var sir_parser = try Sir.Parser.init(self.allocator, self.compilation.env, compilation_file.buffer);
    defer sir_parser.deinit();

    sir_parser.parse() catch |err| {
        switch (err) {
            error.OutOfMemory => std.debug.print("Error: {s}\n", .{root.Cli.errorDescription(err)}),

            else => std.debug.print("{s}:{}:{}: {s}\n", .{
                compilation_file.path,
                sir_parser.error_info.?.source_loc.line,
                sir_parser.error_info.?.source_loc.column,
                sir_parser.error_info.?.message,
            }),
        }

        self.error_info = .{ .message = "import file failed in parsing pass", .source_loc = SourceLoc.find(self.file.buffer, file_path.token_start) };

        return error.ImportFailed;
    };

    var sema = try Sema.init(self.allocator, compilation_file, self.compilation);
    defer sema.deinit();

    sema.analyze(sir_parser.sir) catch |err| {
        switch (err) {
            error.OutOfMemory => std.debug.print("Error: {s}\n", .{root.Cli.errorDescription(err)}),

            else => std.debug.print("{s}:{}:{}: {s}\n", .{
                compilation_file.path,
                sema.error_info.?.source_loc.line,
                sema.error_info.?.source_loc.column,
                sema.error_info.?.message,
            }),
        }

        self.error_info = .{ .message = "import file failed in semantic analysis pass", .source_loc = SourceLoc.find(self.file.buffer, file_path.token_start) };

        return error.ImportFailed;
    };

    sir_parser.sir.instructions.deinit(self.allocator);

    // We should be in global scope if the semantic analysis was successful
    std.debug.assert(sema.scope.maybe_parent == null);

    var variable_entry_iterator = sema.scope.items.iterator();

    while (variable_entry_iterator.next()) |variable_entry| {
        var variable = variable_entry.value_ptr.*;
        var variable_name = variable_entry.key_ptr.*;

        variable_name = if (import_root) blk: {
            if (variable.maybe_prefixed == null) variable.maybe_prefixed = variable_name;
            break :blk try prefix(self.allocator, "root", variable_name);
        } else if (variable.maybe_prefixed) |prefixed_name|
            prefixed_name
        else blk: {
            variable.maybe_prefixed = variable_name;
            break :blk try prefix(self.allocator, sema.module, variable_name);
        };

        try self.scope.put(self.allocator, variable_name, variable);
    }

    try self.airs.appendSlice(self.allocator, sema.airs.items);

    sema.airs.deinit(self.allocator);
}

pub fn analyze(self: *Sema, sir: Sir) Error!void {
    var type_aliases: std.StringHashMapUnmanaged(Sir.SubSymbol) = .{};
    defer type_aliases.deinit(self.allocator);

    var externals: std.ArrayListUnmanaged(Sir.SubSymbol) = .{};
    defer externals.deinit(self.allocator);

    var functions: std.ArrayListUnmanaged(struct { Sir.SubSymbol.MaybeExported, []const Sir.Instruction }) = .{};
    defer functions.deinit(self.allocator);

    var global_instructions: std.ArrayListUnmanaged(Sir.Instruction) = .{};
    defer global_instructions.deinit(self.allocator);

    try global_instructions.ensureTotalCapacity(self.allocator, sir.instructions.items.len);

    var maybe_module: ?[]const u8 = null;

    var i: usize = 0;

    while (i < sir.instructions.items.len) : (i += 1) {
        const sir_instruction = sir.instructions.items[i];

        switch (sir_instruction) {
            .type_alias => |subsymbol| {
                if (type_aliases.get(subsymbol.name.buffer) != null) return self.reportRedeclaration(subsymbol.name);
                try type_aliases.put(self.allocator, subsymbol.name.buffer, subsymbol);
            },

            .external => |subsymbol| try externals.append(self.allocator, subsymbol),

            .function => |function| {
                const start = i + 1;
                var end = i + 1;

                var scope_depth: usize = 0;

                for (sir.instructions.items[start..]) |other_instruction| {
                    end += 1;
                    i += 1;

                    switch (other_instruction) {
                        .start_scope => scope_depth += 1,
                        .end_scope => {
                            scope_depth -= 1;
                            if (scope_depth == 0) break;
                        },

                        else => {},
                    }
                }

                try functions.append(self.allocator, .{ function, sir.instructions.items[start..end] });
            },

            .module => |name| {
                if (maybe_module == null) {
                    maybe_module = name.buffer;
                } else {
                    self.error_info = .{ .message = "declaring multiple modules in one file is not allowed", .source_loc = SourceLoc.find(self.file.buffer, name.token_start) };

                    return error.MultipleModules;
                }
            },

            .import => |file_path| try self.import(file_path),

            else => global_instructions.appendAssumeCapacity(sir_instruction),
        }
    }

    self.module = maybe_module orelse {
        self.error_info = .{ .message = "unnamed modules are not allowed", .source_loc = SourceLoc.find(self.file.buffer, 0) };

        return error.UnnamedModule;
    };

    {
        var type_alias_iterator = type_aliases.valueIterator();

        // First Type Alias Pass: put type aliases in scope
        while (type_alias_iterator.next()) |type_alias| {
            if (self.scope.get(type_alias.name.buffer) != null) return self.reportRedeclaration(type_alias.name);
            try self.scope.put(self.allocator, type_alias.name.buffer, .{
                .type = .void,
                .linkage = type_alias.linkage,
                .is_type_alias = true,
            });
        }

        type_alias_iterator = type_aliases.valueIterator();

        // Second Type Alias Pass: analyze type aliases that don't point to others
        while (type_alias_iterator.next()) |type_alias| {
            if (type_alias.subtype == .@"enum" or
                checkTypeAliasPointsToOthers(type_alias.subtype, &type_aliases)) continue;

            try self.checkTypeAliasCircular(type_alias.name, type_alias.subtype, &type_aliases);

            const variable = self.scope.getPtr(type_alias.name.buffer).?;
            variable.type = try self.analyzeSubType(type_alias.subtype);
            variable.linkage = type_alias.linkage;
        }

        type_alias_iterator = type_aliases.valueIterator();

        // Third Type Alias Pass: analyze enums
        while (type_alias_iterator.next()) |type_alias| {
            if (type_alias.subtype != .@"enum") continue;

            try self.checkTypeAliasCircular(type_alias.name, type_alias.subtype, &type_aliases);

            const @"enum" = type_alias.subtype.@"enum";
            const enum_type = try self.analyzeSubType(@"enum".subtype.*);

            try self.checkIntType(enum_type, @"enum".token_start);

            for (@"enum".fields) |field| {
                const enum_field_value: Value = .{ .int = field.value };

                try self.checkUnaryImplicitCast(enum_field_value, enum_type, field.name.token_start);

                const enum_field_entry = try std.mem.concat(self.allocator, u8, &.{ type_alias.name.buffer, "::", field.name.buffer });

                if (self.scope.get(enum_field_entry) != null)
                    return self.reportRedeclaration(.{ .buffer = enum_field_entry, .token_start = field.name.token_start });

                try self.scope.put(self.allocator, enum_field_entry, .{
                    .type = enum_type,
                    .linkage = type_alias.linkage,
                    .maybe_value = enum_field_value,
                    .is_const = true,
                    .is_comptime = true,
                });
            }

            const variable = self.scope.getPtr(type_alias.name.buffer).?;
            variable.type = enum_type;
            variable.linkage = type_alias.linkage;
        }

        type_alias_iterator = type_aliases.valueIterator();

        // Fourth Type Alias Pass: analyze type aliases that isn't an enum
        while (type_alias_iterator.next()) |type_alias| {
            if (type_alias.subtype == .@"enum") continue;

            try self.checkTypeAliasCircular(type_alias.name, type_alias.subtype, &type_aliases);

            const variable = self.scope.getPtr(type_alias.name.buffer).?;
            variable.type = try self.analyzeSubType(type_alias.subtype);
            variable.linkage = type_alias.linkage;
        }
    }

    const global_air = try self.airs.addOne(self.allocator);
    global_air.* = .{};
    self.air = global_air;

    for (global_instructions.items) |global_instruction| {
        try self.analyzeInstruction(global_instruction);
    }

    for (externals.items) |external| {
        if (self.scope.get(external.name.buffer) != null) return self.reportRedeclaration(external.name);

        const symbol = try self.analyzeSubSymbol(external);

        try self.scope.put(self.allocator, external.name.buffer, .{
            .type = symbol.type,
            .linkage = symbol.linkage,
        });

        try self.air.instructions.append(self.allocator, .{ .external = symbol });
    }

    for (functions.items) |entry| {
        const function, _ = entry;

        if (self.scope.get(function.subsymbol.name.buffer) != null) return self.reportRedeclaration(function.subsymbol.name);

        const symbol = try self.analyzeSubSymbol(function.subsymbol);

        try self.scope.put(self.allocator, symbol.name.buffer, .{
            .maybe_prefixed = if (!function.exported) try prefix(self.allocator, self.module, symbol.name.buffer) else null,
            .type = symbol.type,
            .linkage = symbol.linkage,
        });
    }

    const functions_air = try self.airs.addOne(self.allocator);
    functions_air.* = .{};
    self.air = functions_air;

    for (functions.items) |entry| {
        var function, const sir_instructions = entry;

        const variable = self.scope.get(function.subsymbol.name.buffer).?;

        if (variable.maybe_prefixed) |prefixed_name| function.subsymbol.name.buffer = prefixed_name;

        try self.air.instructions.append(self.allocator, .{
            .function = .{
                .symbol = .{
                    .name = function.subsymbol.name,
                    .type = variable.type,
                    .linkage = variable.linkage,
                },
                .exported = function.exported,
            },
        });

        self.maybe_function_type = variable.type;

        for (sir_instructions) |sir_instruction| {
            try self.analyzeInstruction(sir_instruction);
        }
    }
}

fn analyzeInstruction(self: *Sema, instruction: Sir.Instruction) Error!void {
    switch (instruction) {
        .duplicate => try self.analyzeDuplicate(),
        .reverse => |count| try self.analyzeReverse(count),
        .pop => try self.analyzePop(),

        .string => |string| try self.analyzeString(string),
        .int => |int| try self.analyzeInt(int),
        .float => |float| try self.analyzeFloat(float),

        .negate => |token_start| try self.analyzeNegate(token_start),

        .bool_not => |token_start| try self.analyzeNot(.bool, token_start),
        .bit_not => |token_start| try self.analyzeNot(.bit, token_start),

        .bit_and => |token_start| try self.analyzeBitwiseArithmetic(.bit_and, token_start),
        .bit_or => |token_start| try self.analyzeBitwiseArithmetic(.bit_or, token_start),
        .bit_xor => |token_start| try self.analyzeBitwiseArithmetic(.bit_xor, token_start),

        .write => |token_start| try self.analyzeWrite(token_start),
        .read => |token_start| try self.analyzeRead(token_start),
        .pre_element => |token_start| try self.analyzePreElement(token_start),
        .element => |token_start| try self.analyzeElement(token_start),
        .field => |name| try self.analyzeField(name),
        .reference => try self.analyzeReference(),

        .add => |token_start| try self.analyzeArithmetic(.add, token_start),
        .sub => |token_start| try self.analyzeArithmetic(.sub, token_start),
        .mul => |token_start| try self.analyzeArithmetic(.mul, token_start),
        .div => |token_start| try self.analyzeArithmetic(.div, token_start),
        .rem => |token_start| try self.analyzeArithmetic(.rem, token_start),

        .lt => |token_start| try self.analyzeComparison(.lt, token_start),
        .gt => |token_start| try self.analyzeComparison(.gt, token_start),
        .eql => |token_start| try self.analyzeComparison(.eql, token_start),

        .shl => |token_start| try self.analyzeBitwiseShift(.left, token_start),
        .shr => |token_start| try self.analyzeBitwiseShift(.right, token_start),

        .cast => |cast| try self.analyzeCast(cast),

        .assembly => |assembly| try self.analyzeAssembly(assembly),

        .call => |call| try self.analyzeCall(call),

        .parameters => |subsymbols| try self.analyzeParameters(subsymbols),

        .constant => |subsymbol| try self.analyzeConstant(subsymbol),

        .variable => |variable| try self.analyzeVariable(false, variable),
        .variable_infer => |subsymbol| try self.analyzeVariable(true, subsymbol),

        .set => |name| try self.analyzeSet(name),
        .get => |name| try self.analyzeGet(name),

        .block => |block| try self.analyzeBlock(block),
        .br => |br| try self.analyzeBr(br),
        .cond_br => |cond_br| try self.analyzeCondBr(cond_br),
        .@"switch" => |@"switch"| try self.analyzeSwitch(@"switch"),

        .start_scope => try self.modifyScope(true),
        .end_scope => try self.modifyScope(false),

        .ret => |token_start| try self.analyzeReturn(true, token_start),
        .ret_void => |token_start| try self.analyzeReturn(false, token_start),

        else => {},
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

fn analyzeNegate(self: *Sema, token_start: u32) Error!void {
    const rhs = self.stack.pop();

    if (!rhs.getType().canBeNegative() and rhs != .int) {
        var error_message_buf: std.ArrayListUnmanaged(u8) = .{};

        try error_message_buf.writer(self.allocator).print("'{}' cannot be negative", .{rhs.getType()});

        self.error_info = .{ .message = error_message_buf.items, .source_loc = SourceLoc.find(self.file.buffer, token_start) };

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

            try self.stack.append(self.allocator, .{ .runtime = rhs_runtime });
        },

        else => unreachable,
    }
}

const NotOperation = enum {
    bool,
    bit,
};

fn analyzeNot(self: *Sema, comptime operand: NotOperation, token_start: u32) Error!void {
    const rhs = self.stack.pop();
    const rhs_type = rhs.getType();

    if (operand == .bool) {
        try self.checkUnaryImplicitCast(rhs, .bool, token_start);
    } else if (operand == .bit) {
        try self.checkInt(rhs_type, token_start);
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

            try self.stack.append(self.allocator, .{ .runtime = rhs_runtime });
        },

        else => unreachable,
    }
}

const BitwiseArithmeticOperation = enum {
    bit_and,
    bit_or,
    bit_xor,
};

fn analyzeBitwiseArithmetic(self: *Sema, comptime operation: BitwiseArithmeticOperation, token_start: u32) Error!void {
    const rhs = self.stack.pop();
    const lhs = self.stack.pop();

    var lhs_type = lhs.getType();
    var rhs_type = rhs.getType();

    try self.checkIntOrBool(lhs_type, token_start);
    try self.checkIntOrBool(rhs_type, token_start);

    try self.checkBinaryImplicitCast(lhs, rhs, &lhs_type, &rhs_type, token_start);

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

        .boolean => |lhs_boolean| switch (rhs) {
            .boolean => |rhs_boolean| {
                const result = switch (operation) {
                    .bit_and => @intFromBool(lhs_boolean) & @intFromBool(rhs_boolean),
                    .bit_or => @intFromBool(lhs_boolean) | @intFromBool(rhs_boolean),
                    .bit_xor => @intFromBool(lhs_boolean) ^ @intFromBool(rhs_boolean),
                } == 1;

                try self.stack.append(self.allocator, .{ .boolean = result });

                _ = self.air.instructions.pop();

                self.air.instructions.items[self.air.instructions.items.len - 1] = .{ .boolean = result };

                return;
            },

            else => {},
        },

        else => {},
    }

    switch (operation) {
        .bit_and => try self.air.instructions.append(self.allocator, .bit_and),
        .bit_or => try self.air.instructions.append(self.allocator, .bit_or),
        .bit_xor => try self.air.instructions.append(self.allocator, .bit_xor),
    }

    try self.stack.append(self.allocator, .{ .runtime = lhs_type });
}

fn analyzeWrite(self: *Sema, token_start: u32) Error!void {
    const lhs = self.stack.pop();
    const lhs_type = lhs.getType();

    const lhs_pointer = lhs_type.getPointer() orelse return self.reportNotPointer(lhs_type, token_start);

    const rhs = self.stack.pop();

    if (lhs_pointer.is_const) {
        self.error_info = .{ .message = "cannot mutate data pointed by this pointer, it points to read-only data", .source_loc = SourceLoc.find(self.file.buffer, token_start) };

        return error.UnexpectedMutation;
    }

    try self.checkUnaryImplicitCast(rhs, lhs_pointer.child_type.*, token_start);

    try self.air.instructions.append(self.allocator, .write);
}

fn analyzeRead(self: *Sema, token_start: u32) Error!void {
    const rhs = self.stack.pop();
    const rhs_type = rhs.getType();

    const rhs_pointer = rhs_type.getPointer() orelse return self.reportNotPointer(rhs_type, token_start);

    if (rhs_pointer.child_type.* == .function) {
        self.error_info = .{ .message = "cannot read from a function pointer, it can only be called", .source_loc = SourceLoc.find(self.file.buffer, token_start) };

        return error.UnexpectedFunctionPointer;
    }

    try self.air.instructions.append(self.allocator, .read);

    try self.stack.append(self.allocator, .{ .runtime = rhs_pointer.child_type.* });
}

fn analyzePreElement(self: *Sema, token_start: u32) Error!void {
    const lhs = self.stack.getLast();
    const lhs_type = lhs.getType();

    if (lhs_type == .array) {
        return self.analyzeReference();
    } else if (lhs_type == .pointer and lhs_type.pointer.size != .many and lhs_type.pointer.child_type.* != .array) {
        return self.reportNotIndexable(lhs_type, token_start);
    }
}

fn analyzeElement(self: *Sema, token_start: u32) Error!void {
    const rhs = self.stack.pop();

    const lhs = self.stack.pop();
    const lhs_type = lhs.getType();

    const usize_type: Type = .{ .int = .{ .signedness = .unsigned, .bits = self.compilation.env.target.ptrBitWidth() } };

    try self.checkUnaryImplicitCast(rhs, usize_type, token_start);

    if (rhs == .int and lhs_type.pointer.child_type.* == .array and
        rhs.int >= lhs_type.pointer.child_type.array.len)
    {
        self.error_info = .{ .message = "index out of bounds", .source_loc = SourceLoc.find(self.file.buffer, token_start) };

        return error.UnexpectedValue;
    }

    try self.air.instructions.append(self.allocator, .get_element_ptr);
    try self.air.instructions.append(self.allocator, .read);

    if (lhs_type.pointer.child_type.* == .array) {
        try self.stack.append(self.allocator, .{ .runtime = lhs_type.pointer.child_type.array.child_type.* });
    } else {
        try self.stack.append(self.allocator, .{ .runtime = lhs_type.pointer.child_type.* });
    }
}

fn analyzeField(self: *Sema, name: Name) Error!void {
    const rhs_type = self.stack.getLast().getType();

    if (rhs_type == .@"struct") try self.analyzeReference();

    const rhs = self.stack.pop();

    if (rhs_type == .array or
        (rhs_type == .pointer and rhs_type.pointer.child_type.* == .array))
    {
        if (std.mem.eql(u8, name.buffer, "len")) {
            const rhs_array = if (rhs_type.getPointer()) |pointer| pointer.child_type.array else rhs_type.array;

            try self.air.instructions.append(self.allocator, .pop);
            try self.air.instructions.append(self.allocator, .{ .int = @intCast(rhs_array.len) });

            return self.stack.append(self.allocator, .{ .int = @intCast(rhs_array.len) });
        }
    } else if (rhs_type == .@"struct" or
        (rhs_type == .pointer and rhs_type.pointer.child_type.* == .@"struct"))
    {
        const rhs_struct = if (rhs_type.getPointer()) |pointer| pointer.child_type.@"struct" else rhs_type.@"struct";

        for (rhs_struct.fields, 0..) |field, i| {
            if (std.mem.eql(u8, field.name, name.buffer)) {
                try self.air.instructions.append(self.allocator, .{ .get_field_ptr = @intCast(i) });
                try self.air.instructions.append(self.allocator, .read);

                return self.stack.append(self.allocator, .{ .runtime = field.type });
            }
        }
    }

    var error_message_buf: std.ArrayListUnmanaged(u8) = .{};

    try error_message_buf.writer(self.allocator).print("'{s}' is not a field in '{}'", .{ name.buffer, rhs });

    self.error_info = .{ .message = error_message_buf.items, .source_loc = SourceLoc.find(self.file.buffer, name.token_start) };

    return error.Undeclared;
}

threadlocal var prng = std.Random.DefaultPrng.init(0);

fn analyzeReference(self: *Sema) Error!void {
    const rhs = self.stack.pop();
    const rhs_type = rhs.getType();

    const last_instruction = self.air.instructions.items[self.air.instructions.items.len - 1];

    if (last_instruction == .read or rhs != .runtime) {
        _ = self.air.instructions.pop();
    }

    if (last_instruction != .read) {
        const anon_var_name = try std.fmt.allocPrint(self.allocator, "compiler::__anon_{}", .{prng.random().int(u32)});

        if (rhs == .runtime) {
            try self.air.instructions.append(
                self.allocator,
                .{
                    .variable = .{
                        .symbol = .{
                            .name = .{ .buffer = anon_var_name, .token_start = 0 },
                            .type = rhs_type,
                            .linkage = .local,
                        },
                        .exported = false,
                    },
                },
            );

            try self.air.instructions.append(self.allocator, .{ .get_variable_ptr = anon_var_name });
            try self.air.instructions.append(self.allocator, .duplicate);
            try self.air.instructions.append(self.allocator, .{ .reverse = 3 });
            try self.air.instructions.append(self.allocator, .{ .reverse = 2 });
            try self.air.instructions.append(self.allocator, .write);
        } else {
            const global_air = &self.airs.items[0];

            try global_air.instructions.append(self.allocator, last_instruction);

            try global_air.instructions.append(
                self.allocator,
                .{
                    .variable = .{
                        .symbol = .{
                            .name = .{ .buffer = anon_var_name, .token_start = 0 },
                            .type = rhs_type,
                            .linkage = .global,
                        },
                        .exported = false,
                    },
                },
            );

            try self.air.instructions.append(self.allocator, .{ .get_variable_ptr = anon_var_name });
        }
    }

    const is_const = false;

    const child_on_heap = try self.allocator.create(Type);
    child_on_heap.* = rhs_type;

    try self.stack.append(self.allocator, .{
        .runtime = .{
            .pointer = .{
                .size = .one,
                .is_const = is_const,
                .child_type = child_on_heap,
            },
        },
    });
}

const ArithmeticOperation = enum {
    add,
    sub,
    mul,
    div,
    rem,
};

fn analyzeArithmetic(self: *Sema, comptime operation: ArithmeticOperation, token_start: u32) Error!void {
    const rhs = self.stack.pop();
    const lhs = self.stack.pop();

    var lhs_type = lhs.getType();
    var rhs_type = rhs.getType();

    if (operation == .add or operation == .sub) {
        try self.checkIntOrFloatOrPointer(lhs_type, token_start);
        try self.checkIntOrFloatOrPointer(rhs_type, token_start);

        const usize_type: Type = .{ .int = .{ .signedness = .unsigned, .bits = self.compilation.env.target.ptrBitWidth() } };

        if (lhs_type == .pointer and rhs_type != .pointer) {
            try self.checkUnaryImplicitCast(rhs, usize_type, token_start);
        } else if (rhs_type == .pointer and lhs_type != .pointer) {
            try self.checkUnaryImplicitCast(lhs, usize_type, token_start);
        } else if (lhs_type == .pointer and rhs_type == .pointer) {
            try self.checkUnaryImplicitCast(lhs, rhs_type, token_start);
        } else {
            try self.checkBinaryImplicitCast(lhs, rhs, &lhs_type, &rhs_type, token_start);
        }
    } else {
        try self.checkIntOrFloat(lhs_type, token_start);
        try self.checkIntOrFloat(rhs_type, token_start);

        try self.checkBinaryImplicitCast(lhs, rhs, &lhs_type, &rhs_type, token_start);
    }

    switch (lhs) {
        .int => |lhs_int| switch (rhs) {
            .int => |rhs_int| {
                if (rhs_int == 0 and (operation == .div or operation == .rem)) {
                    self.error_info = .{ .message = "division by zero", .source_loc = SourceLoc.find(self.file.buffer, token_start) };

                    return error.UnexpectedValue;
                }

                const result = switch (operation) {
                    .add => lhs_int + rhs_int,
                    .sub => lhs_int - rhs_int,
                    .mul => lhs_int * rhs_int,
                    .div => @divFloor(lhs_int, rhs_int),
                    .rem => @rem(lhs_int, rhs_int),
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
                if (rhs_float == 0 and (operation == .div or operation == .rem)) {
                    self.error_info = .{ .message = "division by zero", .source_loc = SourceLoc.find(self.file.buffer, token_start) };

                    return error.UnexpectedValue;
                }

                const result = switch (operation) {
                    .add => lhs_float + rhs_float,
                    .sub => lhs_float - rhs_float,
                    .mul => lhs_float * rhs_float,
                    .div => lhs_float / rhs_float,
                    .rem => @rem(lhs_float, rhs_float),
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
        try self.stack.append(self.allocator, .{ .runtime = lhs_type });
    } else if (rhs_type == .pointer) {
        try self.stack.append(self.allocator, .{ .runtime = rhs_type });
    } else {
        try self.stack.append(self.allocator, .{ .runtime = lhs_type });
    }

    switch (operation) {
        .add => try self.air.instructions.append(self.allocator, .add),
        .sub => try self.air.instructions.append(self.allocator, .sub),
        .mul => try self.air.instructions.append(self.allocator, .mul),
        .div => try self.air.instructions.append(self.allocator, .div),
        .rem => try self.air.instructions.append(self.allocator, .rem),
    }
}

const ComparisonOperation = enum {
    lt,
    gt,
    eql,
};

fn analyzeComparison(self: *Sema, comptime operation: ComparisonOperation, token_start: u32) Error!void {
    const rhs = self.stack.pop();
    const lhs = self.stack.pop();

    var lhs_type = lhs.getType();
    var rhs_type = rhs.getType();

    if (operation == .lt or operation == .gt) {
        try self.checkIntOrFloat(lhs_type, token_start);
        try self.checkIntOrFloat(rhs_type, token_start);
    }

    try self.checkCanBeCompared(lhs_type, token_start);
    try self.checkCanBeCompared(rhs_type, token_start);

    try self.checkBinaryImplicitCast(lhs, rhs, &lhs_type, &rhs_type, token_start);

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
        .lt => try self.air.instructions.append(self.allocator, .lt),
        .gt => try self.air.instructions.append(self.allocator, .gt),
        .eql => try self.air.instructions.append(self.allocator, .eql),
    }

    try self.stack.append(self.allocator, .{ .runtime = .bool });
}

const BitwiseShiftDirection = enum {
    left,
    right,
};

fn analyzeBitwiseShift(self: *Sema, comptime direction: BitwiseShiftDirection, token_start: u32) Error!void {
    const rhs = self.stack.pop();
    const lhs = self.stack.pop();

    const lhs_type = lhs.getType();
    const rhs_type = rhs.getType();

    try self.checkInt(lhs_type, token_start);
    try self.checkInt(rhs_type, token_start);

    if (lhs != .runtime and rhs != .runtime) {
        const lhs_int = lhs.int;
        const rhs_int = rhs.int;

        const count_type: Type = .{ .int = .{ .signedness = .unsigned, .bits = 7 } };

        try self.checkBitShiftCount(rhs, count_type, token_start);

        const result = switch (direction) {
            .left => lhs_int << @intCast(rhs_int),
            .right => lhs_int >> @intCast(rhs_int),
        };

        try self.stack.append(self.allocator, .{ .int = result });

        _ = self.air.instructions.pop();

        self.air.instructions.items[self.air.instructions.items.len - 1] = .{ .int = result };
    } else {
        const count_type: Type = .{ .int = .{ .signedness = .unsigned, .bits = std.math.log2(lhs_type.int.bits) } };

        try self.checkBitShiftCount(rhs, count_type, token_start);

        switch (direction) {
            .left => try self.air.instructions.append(self.allocator, .shl),
            .right => try self.air.instructions.append(self.allocator, .shr),
        }

        try self.stack.append(self.allocator, .{ .runtime = lhs_type });
    }
}

fn analyzeCast(self: *Sema, cast: Sir.Instruction.Cast) Error!void {
    const from = self.stack.getLast().getType();
    const to = try self.analyzeSubType(cast.to);

    if (from.eql(to)) return;

    const rhs = self.stack.pop();

    if (to == .void) {
        self.error_info = .{ .message = "cannot cast to 'void' as it is not possible to represent a value of this type", .source_loc = SourceLoc.find(self.file.buffer, cast.token_start) };

        return error.UnexpectedType;
    } else if (to == .function) {
        self.error_info = .{ .message = "cannot cast to a function type as it should be always wrapped in a pointer", .source_loc = SourceLoc.find(self.file.buffer, cast.token_start) };

        return error.UnexpectedType;
    } else if (to == .@"struct" or from == .@"struct") {
        self.error_info = .{ .message = "cannot cast from or to a struct", .source_loc = SourceLoc.find(self.file.buffer, cast.token_start) };

        return error.UnexpectedType;
    } else if (to == .array or from == .array) {
        self.error_info = .{ .message = "cannot cast from or to an array", .source_loc = SourceLoc.find(self.file.buffer, cast.token_start) };

        return error.UnexpectedType;
    } else if (to == .pointer and from != .pointer) {
        const usize_type: Type = .{ .int = .{ .signedness = .unsigned, .bits = self.compilation.env.target.ptrBitWidth() } };

        try self.checkUnaryImplicitCast(rhs, usize_type, cast.token_start);
    } else if (from == .pointer and to != .pointer) {
        const usize_type: Type = .{ .int = .{ .signedness = .unsigned, .bits = self.compilation.env.target.ptrBitWidth() } };

        if (!to.eql(usize_type)) {
            var error_message_buf: std.ArrayListUnmanaged(u8) = .{};

            // zig fmt: off
            try error_message_buf.writer(self.allocator).print(
                "cannot cast from a pointer to '{}', pointers can only cast to '{}' in this target " ++
                "(note: you should use 'usize' as it is an alias for '{}' in cross compilable manner as the size is different in other targets)",
                .{ to, usize_type, usize_type },
            );
            // zig fmt: on

            self.error_info = .{ .message = error_message_buf.items, .source_loc = SourceLoc.find(self.file.buffer, cast.token_start) };

            return error.UnexpectedType;
        }
    } else if (to == .bool) {
        self.error_info = .{ .message = "cannot cast to a boolean, use comparison instead", .source_loc = SourceLoc.find(self.file.buffer, cast.token_start) };

        return error.UnexpectedType;
    } else if (from == .bool) {
        try self.checkInt(to, cast.token_start);
    } else if (from == .float) {
        try self.checkIntOrFloat(to, cast.token_start);
    }

    try self.air.instructions.append(self.allocator, .{ .cast = to });

    try self.stack.append(self.allocator, .{ .runtime = to });
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

        try self.stack.append(self.allocator, .{ .runtime = output_constraint_type });
    } else {
        try self.air.instructions.append(self.allocator, .{
            .assembly = .{
                .content = assembly.content,
                .input_constraints = assembly.input_constraints,
                .output_constraint = null,
                .clobbers = assembly.clobbers,
            },
        });

        try self.stack.append(self.allocator, .{ .runtime = .void });
    }
}

fn analyzeCall(self: *Sema, call: Sir.Instruction.Call) Error!void {
    const callable = self.stack.pop();
    const callable_type = callable.getType();

    var error_message_buf: std.ArrayListUnmanaged(u8) = .{};

    if (callable_type.getFunction()) |function| {
        if ((function.is_var_args and function.parameter_types.len > call.arguments_count) or
            (!function.is_var_args and function.parameter_types.len != call.arguments_count))
        {
            try error_message_buf.writer(self.allocator).print("expected {} argument(s) got {} argument(s)", .{ function.parameter_types.len, call.arguments_count });

            self.error_info = .{ .message = error_message_buf.items, .source_loc = SourceLoc.find(self.file.buffer, call.token_start) };

            return error.UnexpectedArgumentsCount;
        }

        for (function.parameter_types) |parameter_type| {
            const argument = self.stack.pop();

            try self.checkUnaryImplicitCast(argument, parameter_type, call.token_start);
        }

        if (function.is_var_args) {
            self.stack.shrinkRetainingCapacity(self.stack.items.len - (call.arguments_count - function.parameter_types.len));
        }

        try self.air.instructions.append(self.allocator, .{ .call = call.arguments_count });

        try self.stack.append(self.allocator, .{ .runtime = function.return_type.* });
    } else {
        try error_message_buf.writer(self.allocator).print("'{}' is not a callable", .{callable_type});

        self.error_info = .{ .message = error_message_buf.items, .source_loc = SourceLoc.find(self.file.buffer, call.token_start) };

        return error.MismatchedTypes;
    }
}

fn analyzeParameters(self: *Sema, subsymbols: []const Sir.SubSymbol) Error!void {
    var symbols: std.ArrayListUnmanaged(Symbol) = .{};
    try symbols.ensureTotalCapacity(self.allocator, subsymbols.len);

    for (subsymbols) |subsymbol| {
        const symbol = try self.analyzeSubSymbol(subsymbol);
        if (self.scope.get(symbol.name.buffer) != null) return self.reportRedeclaration(symbol.name);

        symbols.appendAssumeCapacity(symbol);

        try self.scope.put(self.allocator, symbol.name.buffer, .{
            .type = symbol.type,
            .linkage = symbol.linkage,
        });
    }

    try self.air.instructions.append(self.allocator, .{ .parameters = try symbols.toOwnedSlice(self.allocator) });
}

fn analyzeConstant(self: *Sema, subsymbol: Sir.SubSymbol) Error!void {
    const symbol = try self.analyzeSubSymbol(subsymbol);
    if (self.scope.get(symbol.name.buffer) != null) return self.reportRedeclaration(symbol.name);

    var variable: Variable = .{
        .type = symbol.type,
        .linkage = symbol.linkage,
        .is_const = true,
        .is_comptime = true,
    };

    var value = self.stack.getLast();

    variable.type = value.getType();

    if (value == .runtime) {
        self.error_info = .{ .message = "expected the constant initializer to be compile time known", .source_loc = SourceLoc.find(self.file.buffer, symbol.name.token_start) };

        return error.ExpectedCompiletimeConstant;
    }

    try self.scope.put(self.allocator, symbol.name.buffer, variable);
}

fn analyzeVariable(self: *Sema, infer: bool, subsymbol_maybe_exported: Sir.SubSymbol.MaybeExported) Error!void {
    var symbol = try self.analyzeSubSymbol(subsymbol_maybe_exported.subsymbol);
    if (self.scope.get(symbol.name.buffer) != null) return self.reportRedeclaration(symbol.name);

    if (infer) {
        symbol.type = self.stack.getLast().getType();
    }

    if (symbol.type == .void) {
        self.error_info = .{ .message = "cannot declare a variable with type 'void'", .source_loc = SourceLoc.find(self.file.buffer, symbol.name.token_start) };

        return error.UnexpectedType;
    }

    var variable: Variable = .{ .type = symbol.type, .linkage = symbol.linkage };

    variable.maybe_prefixed = if (!subsymbol_maybe_exported.exported) try prefix(self.allocator, self.module, symbol.name.buffer) else null;

    try self.scope.put(self.allocator, symbol.name.buffer, variable);

    if (variable.maybe_prefixed) |prefixed_name| symbol.name.buffer = prefixed_name;

    try self.air.instructions.append(self.allocator, .{ .variable = .{ .symbol = symbol, .exported = subsymbol_maybe_exported.exported } });
}

fn analyzeSet(self: *Sema, name: Name) Error!void {
    const variable = self.scope.getPtr(name.buffer) orelse return self.reportNotDeclared(name);
    if (variable.is_type_alias) return self.reportTypeNotExpression(name);

    const value = self.stack.pop();

    try self.checkUnaryImplicitCast(value, variable.type, name.token_start);

    if (variable.is_comptime and variable.maybe_value == null) {
        _ = self.air.instructions.pop();

        variable.maybe_value = value;
    } else if (variable.is_const) {
        self.error_info = .{ .message = "cannot mutate the value of a constant", .source_loc = SourceLoc.find(self.file.buffer, name.token_start) };

        return error.UnexpectedMutation;
    } else if (variable.linkage == .global and value == .runtime and self.maybe_function_type == null) {
        self.error_info = .{ .message = "expected global variable initializer to be compile time known", .source_loc = SourceLoc.find(self.file.buffer, name.token_start) };

        return error.ExpectedCompiletimeConstant;
    } else if (self.maybe_function_type != null) {
        try self.air.instructions.append(self.allocator, .{ .get_variable_ptr = variable.maybe_prefixed orelse name.buffer });
        try self.air.instructions.append(self.allocator, .write);
    }
}

fn analyzeGet(self: *Sema, name: Name) Error!void {
    const variable = self.scope.get(name.buffer) orelse return self.reportNotDeclared(name);
    if (variable.is_type_alias) return self.reportTypeNotExpression(name);

    if (variable.maybe_value) |value| {
        switch (value) {
            .string => |string| try self.air.instructions.append(self.allocator, .{ .string = string }),
            .int => |int| try self.air.instructions.append(self.allocator, .{ .int = int }),
            .float => |float| try self.air.instructions.append(self.allocator, .{ .float = float }),
            .boolean => |boolean| try self.air.instructions.append(self.allocator, .{ .boolean = boolean }),
            .runtime => unreachable,
        }

        try self.stack.append(self.allocator, value);
    } else {
        try self.air.instructions.append(self.allocator, .{ .get_variable_ptr = variable.maybe_prefixed orelse name.buffer });

        if (variable.type.getFunction() == null) {
            try self.air.instructions.append(self.allocator, .read);
        }

        try self.stack.append(self.allocator, .{ .runtime = variable.type });
    }
}

fn analyzeBlock(self: *Sema, id: u32) Error!void {
    try self.air.instructions.append(self.allocator, .{ .block = id });
}

fn analyzeBr(self: *Sema, id: u32) Error!void {
    try self.air.instructions.append(self.allocator, .{ .br = id });
}

fn analyzeCondBr(self: *Sema, cond_br: Sir.Instruction.CondBr) Error!void {
    const condition = self.stack.pop();

    try self.checkUnaryImplicitCast(condition, .bool, cond_br.token_start);

    try self.air.instructions.append(
        self.allocator,
        .{
            .cond_br = .{
                .true_id = cond_br.true_id,
                .false_id = cond_br.false_id,
            },
        },
    );
}

fn analyzeSwitch(self: *Sema, @"switch": Sir.Instruction.Switch) Error!void {
    const switched_value = self.stack.pop();
    const switched_value_type = switched_value.getType();

    try self.checkIntOrBool(switched_value_type, @"switch".token_start);

    var case_values: std.AutoHashMapUnmanaged(i128, void) = .{};

    for (@"switch".case_token_starts) |case_token_start| {
        const case_value = self.stack.pop();

        try self.checkUnaryImplicitCast(case_value, switched_value_type, case_token_start);

        if (case_value == .runtime) {
            self.error_info = .{ .message = "expected switch case value to be compile time known", .source_loc = SourceLoc.find(self.file.buffer, case_token_start) };

            return error.ExpectedCompiletimeConstant;
        }

        const case_value_int = switch (case_value) {
            .int => |int| int,
            .boolean => |boolean| @as(i128, @intFromBool(boolean)),

            else => unreachable,
        };

        if (case_values.get(case_value_int) != null) {
            self.error_info = .{ .message = "duplcicte switch case", .source_loc = SourceLoc.find(self.file.buffer, case_token_start) };

            return error.Redeclared;
        }

        try case_values.put(self.allocator, case_value_int, {});
    }

    case_values.deinit(self.allocator);

    try self.air.instructions.append(self.allocator, .{
        .@"switch" = .{
            .case_block_ids = @"switch".case_block_ids,
            .else_block_id = @"switch".else_block_id,
        },
    });
}

fn modifyScope(self: *Sema, start: bool) Error!void {
    if (start) {
        const local_scope = try self.scopes.addOne(self.allocator);
        local_scope.* = .{ .maybe_parent = self.scope };
        self.scope = local_scope;

        try self.air.instructions.append(self.allocator, .start_scope);
    } else {
        self.scope.deinit(self.allocator);
        self.scope = self.scope.maybe_parent.?;
        _ = self.scopes.pop();

        try self.air.instructions.append(self.allocator, .end_scope);
    }
}

fn analyzeReturn(self: *Sema, with_value: bool, token_start: u32) Error!void {
    std.debug.assert(self.maybe_function_type != null);

    const return_type = self.maybe_function_type.?.pointer.child_type.*.function.return_type.*;

    if (with_value) {
        try self.checkUnaryImplicitCast(self.stack.pop(), return_type, token_start);
    } else {
        if (return_type != .void) {
            self.error_info = .{ .message = "function with non void return type returns void", .source_loc = SourceLoc.find(self.file.buffer, token_start) };

            return error.ExpectedExplicitReturn;
        }
    }

    try self.air.instructions.append(self.allocator, if (with_value) .ret else .ret_void);
}

fn analyzeSubType(self: *Sema, subtype: Sir.SubType) Error!Type {
    switch (subtype) {
        .name => |name| {
            if (self.scope.get(name.buffer)) |variable| {
                if (variable.is_type_alias) {
                    return variable.type;
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
                    .is_var_args = function.is_var_args,
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
                                .child_type = &child_subtype_variable.type,
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

        .array => |array| {
            if (array.child_subtype.* == .name) {
                const child_subtype_name = array.child_subtype.name;

                if (self.scope.getPtr(child_subtype_name.buffer)) |child_subtype_variable| {
                    if (child_subtype_variable.is_type_alias) {
                        return Type{
                            .array = .{
                                .len = array.len,
                                .child_type = &child_subtype_variable.type,
                            },
                        };
                    }
                }

                try self.reportTypeNotDeclared(child_subtype_name);

                unreachable;
            } else {
                const child_type = try self.analyzeSubType(array.child_subtype.*);

                const child_type_on_heap = try self.allocator.create(Type);
                child_type_on_heap.* = child_type;

                return Type{
                    .array = .{
                        .len = array.len,
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
            self.error_info = .{ .message = "enums should be in a type alias as they require a namespace", .source_loc = SourceLoc.find(self.file.buffer, @"enum".token_start) };

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

fn checkTypeAliasCircular(self: *Sema, target: Name, subtype: Sir.SubType, type_aliases: *std.StringHashMapUnmanaged(Sir.SubSymbol)) Error!void {
    switch (subtype) {
        .name => |name| {
            // type I = I;
            //
            // type T = J;
            // type J = T;
            if (std.mem.eql(u8, name.buffer, target.buffer)) {
                return self.reportCircularDependency(target);
            } else if (type_aliases.get(name.buffer)) |type_alias| {
                try self.checkTypeAliasCircular(target, type_alias.subtype, type_aliases);
            }
        },

        .@"struct" => |@"struct"| {
            // type T = struct { t T };
            //
            // type I = struct { j J };
            // type J = struct { i I };
            for (@"struct".subsymbols) |field_subsymbol| {
                try self.checkTypeAliasCircular(target, field_subsymbol.subtype, type_aliases);
            }
        },

        .@"enum" => |@"enum"| {
            // type T = enum T {};
            try self.checkTypeAliasCircular(target, @"enum".subtype.*, type_aliases);
        },

        else => {},
    }
}

fn checkTypeAliasPointsToOthers(subtype: Sir.SubType, type_aliases: *std.StringHashMapUnmanaged(Sir.SubSymbol)) bool {
    return switch (subtype) {
        // Pointers or Arrays
        .pure, .pointer, .array => false,

        .name => |name| type_aliases.get(name.buffer) != null,

        .@"enum" => |@"enum"| checkTypeAliasPointsToOthers(@"enum".subtype.*, type_aliases),

        .function => |function| blk: {
            for (function.parameter_subtypes) |parameter_subtype|
                if (checkTypeAliasPointsToOthers(parameter_subtype, type_aliases))
                    break :blk true;

            break :blk checkTypeAliasPointsToOthers(function.return_subtype.*, type_aliases);
        },

        .@"struct" => |@"struct"| blk: {
            for (@"struct".subsymbols) |field_subsymbol|
                if (checkTypeAliasPointsToOthers(field_subsymbol.subtype, type_aliases))
                    break :blk true;

            break :blk false;
        },
    };
}

fn canUnaryImplicitCast(lhs: Value, to: Type) bool {
    const lhs_type = lhs.getType();

    return (lhs_type.eql(to) or
        (lhs == .int and to == .int and lhs.int >= to.minInt() and
        lhs == .int and to == .int and lhs.int <= to.maxInt()) or
        (lhs == .float and to == .float and lhs.float >= -to.maxFloat() and
        lhs == .float and to == .float and lhs.float <= to.maxFloat()) or
        (lhs_type == .int and to == .int and
        lhs_type.maxInt() <= to.maxInt() and lhs_type.minInt() >= to.minInt() and
        lhs_type.canBeNegative() == to.canBeNegative()) or
        (lhs_type == .float and to == .float and
        lhs_type.maxFloat() <= to.maxFloat()) or
        (lhs_type == .pointer and to == .pointer and
        lhs_type.pointer.child_type.* == .array and to.pointer.size == .many and
        lhs_type.pointer.child_type.array.child_type.eql(to.pointer.child_type.*)));
}

fn checkUnaryImplicitCast(self: *Sema, lhs: Value, to: Type, token_start: u32) Error!void {
    if (!canUnaryImplicitCast(lhs, to)) {
        var error_message_buf: std.ArrayListUnmanaged(u8) = .{};

        try error_message_buf.writer(self.allocator).print("'{}' cannot be implicitly casted to '{}'", .{ lhs.getType(), to });

        self.error_info = .{ .message = error_message_buf.items, .source_loc = SourceLoc.find(self.file.buffer, token_start) };

        return error.MismatchedTypes;
    }
}

fn checkBitShiftCount(self: *Sema, rhs: Value, count_type: Type, token_start: u32) Error!void {
    if (!canUnaryImplicitCast(rhs, count_type)) {
        var error_message_buf: std.ArrayListUnmanaged(u8) = .{};

        try error_message_buf.writer(self.allocator).print("'{}' cannot be used as a bit shift count, as it cannot be implicitly casted to '{}'", .{ rhs, count_type });

        self.error_info = .{ .message = error_message_buf.items, .source_loc = SourceLoc.find(self.file.buffer, token_start) };

        return error.MismatchedTypes;
    }
}

fn checkBinaryImplicitCast(self: *Sema, lhs: Value, rhs: Value, lhs_type: *Type, rhs_type: *Type, token_start: u32) Error!void {
    if (std.meta.activeTag(lhs_type.*) == std.meta.activeTag(rhs_type.*)) {
        if (lhs == .runtime and rhs == .runtime and
            lhs_type.canBeNegative() != rhs_type.canBeNegative())
        {
            try self.reportIncompatibleTypes(lhs_type.*, rhs_type.*, token_start);
        }

        if (lhs_type.* == .int and lhs_type.int.bits > rhs_type.int.bits or
            lhs_type.* == .float and lhs_type.float.bits > rhs_type.float.bits)
        {
            // lhs as u64 > rhs as u16
            // lhs as f64 > rhs as f32
            // lhs as f64 > rhs as f16
            try self.checkUnaryImplicitCast(rhs, lhs_type.*, token_start);

            rhs_type.* = lhs_type.*;
        } else if (lhs_type.* == .int and lhs_type.int.bits < rhs_type.int.bits or
            lhs_type.* == .float and lhs_type.float.bits < rhs_type.float.bits)
        {
            // lhs as u16 > rhs as u64
            // lhs as f32 > rhs as f64
            // lhs as f16 > rhs as f64
            try self.checkUnaryImplicitCast(lhs, rhs_type.*, token_start);

            lhs_type.* = rhs_type.*;
        } else if (lhs_type.* == .pointer) {
            // lhs as *const u8 == rhs as *const u8
            // lhs as *const u8 == rhs as *const u16
            //
            // Both are allowed since it is a pointer comparison which compares the addresses
        }
    } else {
        try self.reportIncompatibleTypes(lhs_type.*, rhs_type.*, token_start);
    }
}

fn checkIntOrBool(self: *Sema, provided_type: Type, token_start: u32) Error!void {
    if (provided_type != .int and provided_type != .bool) {
        var error_message_buf: std.ArrayListUnmanaged(u8) = .{};

        try error_message_buf.writer(self.allocator).print("'{}' is provided while expected an integer or boolean", .{provided_type});
        self.error_info = .{ .message = error_message_buf.items, .source_loc = SourceLoc.find(self.file.buffer, token_start) };

        return error.MismatchedTypes;
    }
}

fn checkIntOrFloat(self: *Sema, provided_type: Type, token_start: u32) Error!void {
    if (provided_type != .int and provided_type != .float) {
        var error_message_buf: std.ArrayListUnmanaged(u8) = .{};

        try error_message_buf.writer(self.allocator).print("'{}' is provided while expected an integer or float", .{provided_type});
        self.error_info = .{ .message = error_message_buf.items, .source_loc = SourceLoc.find(self.file.buffer, token_start) };

        return error.MismatchedTypes;
    }
}

fn checkIntOrFloatOrPointer(self: *Sema, provided_type: Type, token_start: u32) Error!void {
    if (provided_type != .int and provided_type != .float and provided_type != .pointer) {
        var error_message_buf: std.ArrayListUnmanaged(u8) = .{};

        try error_message_buf.writer(self.allocator).print("'{}' is provided while expected an integer or float or pointer", .{provided_type});

        self.error_info = .{ .message = error_message_buf.items, .source_loc = SourceLoc.find(self.file.buffer, token_start) };

        return error.MismatchedTypes;
    }
}

fn checkInt(self: *Sema, provided_type: Type, token_start: u32) Error!void {
    if (provided_type != .int) {
        var error_message_buf: std.ArrayListUnmanaged(u8) = .{};

        try error_message_buf.writer(self.allocator).print("'{}' is provided while expected an integer", .{provided_type});

        self.error_info = .{ .message = error_message_buf.items, .source_loc = SourceLoc.find(self.file.buffer, token_start) };

        return error.MismatchedTypes;
    }
}

fn checkIntType(self: *Sema, provided_type: Type, token_start: u32) Error!void {
    if (provided_type != .int) {
        var error_message_buf: std.ArrayListUnmanaged(u8) = .{};

        try error_message_buf.writer(self.allocator).print("'{}' is provided while expected an integer type", .{provided_type});

        self.error_info = .{ .message = error_message_buf.items, .source_loc = SourceLoc.find(self.file.buffer, token_start) };

        return error.MismatchedTypes;
    }
}

fn checkCanBeCompared(self: *Sema, provided_type: Type, token_start: u32) Error!void {
    if (provided_type == .@"struct" or provided_type == .void or provided_type == .function) {
        return self.reportNotComparable(provided_type, token_start);
    }
}

fn reportIncompatibleTypes(self: *Sema, lhs: Type, rhs: Type, token_start: u32) Error!void {
    var error_message_buf: std.ArrayListUnmanaged(u8) = .{};

    try error_message_buf.writer(self.allocator).print("'{}' is not compatible with '{}'", .{ lhs, rhs });

    self.error_info = .{ .message = error_message_buf.items, .source_loc = SourceLoc.find(self.file.buffer, token_start) };

    return error.MismatchedTypes;
}

fn reportNotDeclared(self: *Sema, name: Name) Error!void {
    var error_message_buf: std.ArrayListUnmanaged(u8) = .{};

    try error_message_buf.writer(self.allocator).print("'{s}' is not declared", .{name.buffer});

    self.error_info = .{ .message = error_message_buf.items, .source_loc = SourceLoc.find(self.file.buffer, name.token_start) };

    return error.Undeclared;
}

fn reportRedeclaration(self: *Sema, name: Name) Error!void {
    var error_message_buf: std.ArrayListUnmanaged(u8) = .{};

    try error_message_buf.writer(self.allocator).print("redeclaration of '{s}'", .{name.buffer});

    self.error_info = .{ .message = error_message_buf.items, .source_loc = SourceLoc.find(self.file.buffer, name.token_start) };

    return error.Redeclared;
}

fn reportTypeNotDeclared(self: *Sema, name: Name) Error!void {
    var error_message_buf: std.ArrayListUnmanaged(u8) = .{};

    try error_message_buf.writer(self.allocator).print("type '{s}' is not declared", .{name.buffer});

    self.error_info = .{ .message = error_message_buf.items, .source_loc = SourceLoc.find(self.file.buffer, name.token_start) };

    return error.Undeclared;
}

fn reportTypeNotExpression(self: *Sema, name: Name) Error!void {
    var error_message_buf: std.ArrayListUnmanaged(u8) = .{};

    try error_message_buf.writer(self.allocator).print("'{s}' is a type not an expression", .{name.buffer});

    self.error_info = .{ .message = error_message_buf.items, .source_loc = SourceLoc.find(self.file.buffer, name.token_start) };

    return error.Undeclared;
}

fn reportNotPointer(self: *Sema, provided_type: Type, token_start: u32) Error!void {
    var error_message_buf: std.ArrayListUnmanaged(u8) = .{};

    try error_message_buf.writer(self.allocator).print("'{}' is not a pointer", .{provided_type});

    self.error_info = .{ .message = error_message_buf.items, .source_loc = SourceLoc.find(self.file.buffer, token_start) };

    return error.MismatchedTypes;
}

fn reportNotIndexable(self: *Sema, provided_type: Type, token_start: u32) Error!void {
    var error_message_buf: std.ArrayListUnmanaged(u8) = .{};

    try error_message_buf.writer(self.allocator).print("'{}' does not support indexing", .{provided_type});

    self.error_info = .{ .message = error_message_buf.items, .source_loc = SourceLoc.find(self.file.buffer, token_start) };

    return error.UnexpectedType;
}

fn reportNotComparable(self: *Sema, provided_type: Type, token_start: u32) Error!void {
    var error_message_buf: std.ArrayListUnmanaged(u8) = .{};

    try error_message_buf.writer(self.allocator).print("'{}' does not support comparison", .{provided_type});

    self.error_info = .{ .message = error_message_buf.items, .source_loc = SourceLoc.find(self.file.buffer, token_start) };

    return error.MismatchedTypes;
}

fn reportCircularDependency(self: *Sema, name: Name) Error!void {
    var error_message_buf: std.ArrayListUnmanaged(u8) = .{};

    try error_message_buf.writer(self.allocator).print("'{s}' is circularly dependent on itself", .{name.buffer});

    self.error_info = .{ .message = error_message_buf.items, .source_loc = SourceLoc.find(self.file.buffer, name.token_start) };

    return error.CircularDependency;
}
