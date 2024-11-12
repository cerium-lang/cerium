const std = @import("std");
const root = @import("root");

const c = @cImport({
    @cInclude("llvm-c/Core.h");
    @cInclude("llvm-c/Target.h");
    @cInclude("llvm-c/TargetMachine.h");
});

const Compilation = @import("../Compilation.zig");
const Air = @import("../Air.zig");
const Symbol = @import("../Symbol.zig");
const Scope = Symbol.Scope;
const Type = Symbol.Type;

const LlvmBackend = @This();

allocator: std.mem.Allocator,

target: std.Target,

context: c.LLVMContextRef,
module: c.LLVMModuleRef,
builder: c.LLVMBuilderRef,

function_value: c.LLVMValueRef = undefined,
function_type: Type = undefined,

basic_blocks: std.AutoHashMapUnmanaged(u32, c.LLVMBasicBlockRef) = .{},

strings: std.StringHashMapUnmanaged(c.LLVMValueRef) = .{},

stack: std.ArrayListUnmanaged(Register) = .{},

scope: *Scope(Variable),
scope_stack: std.ArrayListUnmanaged(Scope(Variable)) = .{},

pub const Error = std.mem.Allocator.Error;

pub const Variable = struct {
    pointer: c.LLVMValueRef,
    type: Type,
    linkage: Symbol.Linkage,
};

pub const Register = struct {
    value: c.LLVMValueRef,
    type: Type,
};

pub fn init(allocator: std.mem.Allocator, target: std.Target) Error!LlvmBackend {
    const context = c.LLVMContextCreate();
    const module = c.LLVMModuleCreateWithNameInContext("module", context);
    const builder = c.LLVMCreateBuilderInContext(context);

    var scope_stack: @FieldType(LlvmBackend, "scope_stack") = .{};

    const global_scope = try scope_stack.addOne(allocator);
    global_scope.* = .{};

    return LlvmBackend{
        .allocator = allocator,
        .target = target,
        .context = context,
        .module = module,
        .builder = builder,
        .scope = global_scope,
        .scope_stack = scope_stack,
    };
}

pub fn deinit(self: *LlvmBackend) void {
    self.basic_blocks.deinit(self.allocator);
    self.strings.deinit(self.allocator);
    self.stack.deinit(self.allocator);
    self.scope_stack.deinit(self.allocator);

    c.LLVMDisposeModule(self.module);
    c.LLVMDisposeBuilder(self.builder);
    c.LLVMContextDispose(self.context);
    c.LLVMShutdown();
}

fn subArchName(features: std.Target.Cpu.Feature.Set, arch: anytype, mappings: anytype) ?[]const u8 {
    inline for (mappings) |mapping| {
        if (arch.featureSetHas(features, mapping[0])) return mapping[1];
    }

    return null;
}

pub fn targetTripleZ(allocator: std.mem.Allocator, target: std.Target) ![:0]u8 {
    var llvm_triple = std.ArrayList(u8).init(allocator);
    defer llvm_triple.deinit();

    const features = target.cpu.features;

    const llvm_arch = switch (target.cpu.arch) {
        .arm => "arm",
        .armeb => "armeb",
        .aarch64 => if (target.abi == .ilp32) "aarch64_32" else "aarch64",
        .aarch64_be => "aarch64_be",
        .arc => "arc",
        .avr => "avr",
        .bpfel => "bpfel",
        .bpfeb => "bpfeb",
        .csky => "csky",
        .hexagon => "hexagon",
        .loongarch32 => "loongarch32",
        .loongarch64 => "loongarch64",
        .m68k => "m68k",
        // MIPS sub-architectures are a bit irregular, so we handle them manually here
        .mips => if (std.Target.mips.featureSetHas(features, .mips32r6)) "mipsisa32r6" else "mips",
        .mipsel => if (std.Target.mips.featureSetHas(features, .mips32r6)) "mipsisa32r6el" else "mipsel",
        .mips64 => if (std.Target.mips.featureSetHas(features, .mips64r6)) "mipsisa64r6" else "mips64",
        .mips64el => if (std.Target.mips.featureSetHas(features, .mips64r6)) "mipsisa64r6el" else "mips64el",
        .msp430 => "msp430",
        .powerpc => "powerpc",
        .powerpcle => "powerpcle",
        .powerpc64 => "powerpc64",
        .powerpc64le => "powerpc64le",
        .amdgcn => "amdgcn",
        .riscv32 => "riscv32",
        .riscv64 => "riscv64",
        .sparc => "sparc",
        .sparc64 => "sparc64",
        .s390x => "s390x",
        .thumb => "thumb",
        .thumbeb => "thumbeb",
        .x86 => "i386",
        .x86_64 => "x86_64",
        .xcore => "xcore",
        .xtensa => "xtensa",
        .nvptx => "nvptx",
        .nvptx64 => "nvptx64",
        .spirv => "spirv",
        .spirv32 => "spirv32",
        .spirv64 => "spirv64",
        .lanai => "lanai",
        .wasm32 => "wasm32",
        .wasm64 => "wasm64",
        .ve => "ve",

        else => unreachable,
    };

    try llvm_triple.appendSlice(llvm_arch);

    const llvm_sub_arch: ?[]const u8 = switch (target.cpu.arch) {
        .arm, .armeb, .thumb, .thumbeb => subArchName(features, std.Target.arm, .{
            .{ .v4t, "v4t" },
            .{ .v5t, "v5t" },
            .{ .v5te, "v5te" },
            .{ .v5tej, "v5tej" },
            .{ .v6, "v6" },
            .{ .v6k, "v6k" },
            .{ .v6kz, "v6kz" },
            .{ .v6m, "v6m" },
            .{ .v6t2, "v6t2" },
            .{ .v7a, "v7a" },
            .{ .v7em, "v7em" },
            .{ .v7m, "v7m" },
            .{ .v7r, "v7r" },
            .{ .v7ve, "v7ve" },
            .{ .v8a, "v8a" },
            .{ .v8_1a, "v8.1a" },
            .{ .v8_2a, "v8.2a" },
            .{ .v8_3a, "v8.3a" },
            .{ .v8_4a, "v8.4a" },
            .{ .v8_5a, "v8.5a" },
            .{ .v8_6a, "v8.6a" },
            .{ .v8_7a, "v8.7a" },
            .{ .v8_8a, "v8.8a" },
            .{ .v8_9a, "v8.9a" },
            .{ .v8m, "v8m.base" },
            .{ .v8m_main, "v8m.main" },
            .{ .v8_1m_main, "v8.1m.main" },
            .{ .v8r, "v8r" },
            .{ .v9a, "v9a" },
            .{ .v9_1a, "v9.1a" },
            .{ .v9_2a, "v9.2a" },
            .{ .v9_3a, "v9.3a" },
            .{ .v9_4a, "v9.4a" },
            .{ .v9_5a, "v9.5a" },
        }),
        .powerpc => subArchName(features, std.Target.powerpc, .{
            .{ .spe, "spe" },
        }),
        .spirv => subArchName(features, std.Target.spirv, .{
            .{ .v1_5, "1.5" },
        }),
        .spirv32, .spirv64 => subArchName(features, std.Target.spirv, .{
            .{ .v1_5, "1.5" },
            .{ .v1_4, "1.4" },
            .{ .v1_3, "1.3" },
            .{ .v1_2, "1.2" },
            .{ .v1_1, "1.1" },
        }),
        else => null,
    };

    if (llvm_sub_arch) |sub| try llvm_triple.appendSlice(sub);

    // Unlike CPU backends, GPU backends actually care about the vendor tag
    try llvm_triple.appendSlice(switch (target.cpu.arch) {
        .amdgcn => if (target.os.tag == .mesa3d) "-mesa-" else "-amd-",
        .nvptx, .nvptx64 => "-nvidia-",
        .spirv64 => if (target.os.tag == .amdhsa) "-amd-" else "-unknown-",
        else => "-unknown-",
    });

    const llvm_os = switch (target.os.tag) {
        .freestanding => "unknown",
        .dragonfly => "dragonfly",
        .freebsd => "freebsd",
        .fuchsia => "fuchsia",
        .linux => "linux",
        .ps3 => "lv2",
        .netbsd => "netbsd",
        .openbsd => "openbsd",
        .solaris, .illumos => "solaris",
        .windows, .uefi => "windows",
        .zos => "zos",
        .haiku => "haiku",
        .rtems => "rtems",
        .aix => "aix",
        .cuda => "cuda",
        .nvcl => "nvcl",
        .amdhsa => "amdhsa",
        .opencl => "unknown", // https://llvm.org/docs/SPIRVUsage.html#target-triples
        .ps4 => "ps4",
        .ps5 => "ps5",
        .elfiamcu => "elfiamcu",
        .mesa3d => "mesa3d",
        .amdpal => "amdpal",
        .hermit => "hermit",
        .hurd => "hurd",
        .wasi => "wasi",
        .emscripten => "emscripten",
        .bridgeos => "bridgeos",
        .macos => "macosx",
        .ios => "ios",
        .tvos => "tvos",
        .watchos => "watchos",
        .driverkit => "driverkit",
        .visionos => "xros",
        .serenity => "serenity",
        .vulkan => "vulkan",

        else => "unknown",
    };
    try llvm_triple.appendSlice(llvm_os);

    if (target.os.tag.isDarwin()) {
        const min_version = target.os.version_range.semver.min;
        try llvm_triple.writer().print("{d}.{d}.{d}", .{
            min_version.major,
            min_version.minor,
            min_version.patch,
        });
    }
    try llvm_triple.append('-');

    const llvm_abi = switch (target.abi) {
        .none, .ilp32 => "unknown",
        .gnu => "gnu",
        .gnuabin32 => "gnuabin32",
        .gnuabi64 => "gnuabi64",
        .gnueabi => "gnueabi",
        .gnueabihf => "gnueabihf",
        .gnuf32 => "gnuf32",
        .gnusf => "gnusf",
        .gnux32 => "gnux32",
        .gnuilp32 => "gnuilp32",
        .code16 => "code16",
        .eabi => "eabi",
        .eabihf => "eabihf",
        .android => "android",
        .androideabi => "androideabi",
        .musl => "musl",
        .muslabin32 => "musl", // Should be "muslabin32" in LLVM 20 (I still use LLVM 17 btw)
        .muslabi64 => "musl", // Should be "muslabi64" in LLVM 20 (I still use LLVM 17 btw)
        .musleabi => "musleabi",
        .musleabihf => "musleabihf",
        .muslx32 => "muslx32",
        .msvc => "msvc",
        .itanium => "itanium",
        .cygnus => "cygnus",
        .simulator => "simulator",
        .macabi => "macabi",
        .ohos => "ohos",
        .ohoseabi => "ohoseabi",
    };
    try llvm_triple.appendSlice(llvm_abi);

    return llvm_triple.toOwnedSliceSentinel(0);
}

pub fn emit(self: *LlvmBackend, output_file_path: [:0]const u8, output_kind: root.OutputKind) Error!void {
    c.LLVMInitializeAllTargetInfos();
    c.LLVMInitializeAllTargets();
    c.LLVMInitializeAllTargetMCs();
    c.LLVMInitializeAllAsmParsers();
    c.LLVMInitializeAllAsmPrinters();

    const target_triple = try targetTripleZ(self.allocator, self.target);
    defer self.allocator.free(target_triple);

    _ = c.LLVMSetTarget(self.module, target_triple);

    var llvm_target: c.LLVMTargetRef = undefined;

    _ = c.LLVMGetTargetFromTriple(target_triple, &llvm_target, null);

    const target_machine = c.LLVMCreateTargetMachine(
        llvm_target,
        target_triple,
        "generic",
        "",
        c.LLVMCodeGenLevelDefault,
        c.LLVMRelocPIC,
        c.LLVMCodeModelDefault,
    );

    _ = c.LLVMTargetMachineEmitToFile(
        target_machine,
        self.module,
        output_file_path,
        if (output_kind == .object or output_kind == .executable) c.LLVMObjectFile else c.LLVMAssemblyFile,
        null,
    );
}

fn getLlvmType(self: *LlvmBackend, @"type": Type) Error!c.LLVMTypeRef {
    return switch (@"type") {
        .void => c.LLVMVoidTypeInContext(self.context),
        .bool => c.LLVMIntTypeInContext(self.context, 1),
        .int => |int| c.LLVMIntTypeInContext(self.context, int.bits),
        .float => |float| if (float.bits == 16)
            c.LLVMHalfTypeInContext(self.context)
        else if (float.bits == 32)
            c.LLVMFloatTypeInContext(self.context)
        else
            c.LLVMDoubleTypeInContext(self.context),
        .pointer => c.LLVMPointerTypeInContext(self.context, 1),

        .function => |function| blk: {
            const parameter_types = try self.allocator.alloc(c.LLVMTypeRef, function.parameter_types.len);

            for (function.parameter_types, 0..) |parameter, i| {
                parameter_types[i] = try self.getLlvmType(parameter);
            }

            const return_type = try self.getLlvmType(function.return_type.*);

            break :blk c.LLVMFunctionType(return_type, parameter_types.ptr, @intCast(parameter_types.len), @intFromBool(function.is_var_args));
        },

        .@"struct" => |@"struct"| blk: {
            const element_types = try self.allocator.alloc(c.LLVMTypeRef, @"struct".fields.len);

            for (@"struct".fields, 0..) |field, i| {
                element_types[i] = try self.getLlvmType(field.type);
            }

            break :blk c.LLVMStructTypeInContext(self.context, element_types.ptr, @intCast(element_types.len), 0);
        },
    };
}

/// Use this instead of `c.LLVMBuildIntCast2`
fn saneIntCast(self: *LlvmBackend, lhs: Register, to: Type) Error!c.LLVMValueRef {
    std.debug.assert(to == .int);
    std.debug.assert(lhs.type == .int or lhs.type == .bool);

    const lhs_int = if (lhs.type == .int) lhs.type.int else Type.Int{ .signedness = .unsigned, .bits = 1 };
    const to_int = to.int;

    // u16 -> u8 (Regular IntCast)
    // u8 -> s8 (Regular IntCast)
    // s8 -> u16 (Regular IntCast)
    // u8 -> s16 (ZExt)
    // u8 -> u16 (ZExt)
    // s8 -> s16 (SExt)

    if (lhs_int.signedness == .unsigned) {
        if (to_int.bits > lhs_int.bits) {
            return c.LLVMBuildZExt(self.builder, lhs.value, try self.getLlvmType(to), "");
        } else {
            return c.LLVMBuildIntCast2(self.builder, lhs.value, try self.getLlvmType(to), @intFromEnum(to_int.signedness), "");
        }
    } else {
        if (lhs_int.signedness == to_int.signedness and to_int.bits > lhs_int.bits) {
            return c.LLVMBuildSExt(self.builder, lhs.value, try self.getLlvmType(to), "");
        } else {
            return c.LLVMBuildIntCast2(self.builder, lhs.value, try self.getLlvmType(to), @intFromEnum(to_int.signedness), "");
        }
    }
}

/// Use this instead of `c.LLVMBuildFPCast`
fn saneFloatCast(self: *LlvmBackend, lhs: Register, to: Type) Error!c.LLVMValueRef {
    return c.LLVMBuildFPCast(self.builder, lhs.value, try self.getLlvmType(to), "");
}

fn unaryImplicitCast(self: *LlvmBackend, lhs: *Register, to: Type) Error!void {
    if (lhs.type.eql(to)) return;

    // var x u8 = 4;
    // var y f32 = 4.0;
    // var z u16 = x;

    if (to == .int) {
        lhs.value = try self.saneIntCast(lhs.*, to);
    } else if (to == .float) {
        lhs.value = try self.saneFloatCast(lhs.*, to);
    }

    lhs.type = to;
}

fn binaryImplicitCast(self: *LlvmBackend, lhs: *Register, rhs: *Register) Error!void {
    if (lhs.type.eql(rhs.type)) return;

    if ((lhs.type == .int and
        lhs.type.int.bits > rhs.type.int.bits) or
        (lhs.type == .float and
        lhs.type.float.bits > rhs.type.float.bits))
    {
        // lhs as u64 > rhs as u16
        // lhs as s64 > rhs as s16
        try self.unaryImplicitCast(rhs, lhs.type);
    } else if ((lhs.type == .int and
        lhs.type.int.bits < rhs.type.int.bits) or
        (lhs.type == .float and
        lhs.type.float.bits < rhs.type.float.bits))
    {
        // lhs as u16 > rhs as u64
        // lhs as s16 > rhs as s64
        try self.unaryImplicitCast(lhs, rhs.type);
    }
}

pub fn render(self: *LlvmBackend, air: Air) Error!void {
    for (air.instructions.items) |air_instruction| {
        switch (air_instruction) {
            .function => |symbol| {
                if (self.scope.get(symbol.name.buffer) != null) continue;

                const function_pointer = c.LLVMAddFunction(
                    self.module,
                    try self.allocator.dupeZ(u8, symbol.name.buffer),
                    try self.getLlvmType(symbol.type.pointer.child_type.*),
                );

                try self.scope.put(
                    self.allocator,
                    symbol.name.buffer,
                    .{
                        .pointer = function_pointer,
                        .type = symbol.type,
                        .linkage = symbol.linkage,
                    },
                );
            },

            else => {},
        }
    }

    for (air.instructions.items, 0..) |air_instruction, i| {
        try self.renderInstruction(air_instruction, air.instructions.items[i..]);
    }
}

fn renderInstruction(
    self: *LlvmBackend,
    air_instruction: Air.Instruction,
    remainder_instructions: []const Air.Instruction,
) Error!void {
    switch (air_instruction) {
        .duplicate => try self.stack.append(self.allocator, self.stack.getLast()),
        .reverse => |count| std.mem.reverse(Register, self.stack.items[self.stack.items.len - count ..]),
        .pop => _ = self.stack.pop(),

        .string => |string| try self.renderString(string),
        .int => |int| try self.renderInt(int),
        .float => |float| try self.renderFloat(float),
        .boolean => |boolean| try self.renderBoolean(boolean),

        .negate => try self.renderNegate(),

        .bool_not, .bit_not => try self.renderNot(),

        .bit_and => try self.renderBitwiseArithmetic(.bit_and),
        .bit_or => try self.renderBitwiseArithmetic(.bit_or),
        .bit_xor => try self.renderBitwiseArithmetic(.bit_xor),

        .write => try self.renderWrite(),
        .read => try self.renderRead(),
        .get_element_ptr => try self.renderGetElementPtr(),
        .get_field_ptr => |field_index| try self.renderGetFieldPtr(field_index),

        .add => try self.renderArithmetic(.add),
        .sub => try self.renderArithmetic(.sub),
        .mul => try self.renderArithmetic(.mul),
        .div => try self.renderArithmetic(.div),
        .rem => try self.renderArithmetic(.rem),

        .cmp => |operation| try self.renderComparison(operation),

        .shl => try self.renderBitwiseShift(.left),
        .shr => try self.renderBitwiseShift(.right),

        .cast => |cast_to| try self.renderCast(cast_to),

        .assembly => |assembly| try self.renderAssembly(assembly),

        .call => |arguments_count| try self.renderCall(arguments_count),

        .function => |symbol| try self.renderFunction(symbol, remainder_instructions),

        .parameters => |symbols| try self.renderParameters(symbols),

        .variable => |symbol| try self.renderVariable(symbol),
        .external => |symbol| try self.renderExternal(symbol),

        .set => |name| try self.renderSet(name),
        .get => |name| try self.renderGet(name),
        .get_ptr => |name| try self.renderGetPtr(name),

        .block => |block| try self.renderBlock(block),
        .br => |br| try self.renderBr(br),
        .cond_br => |cond_br| try self.renderCondBr(cond_br),

        .start_scope => try self.modifyScope(true),
        .end_scope => try self.modifyScope(false),

        .ret => try self.renderReturn(true),
        .ret_void => try self.renderReturn(false),
    }
}

fn renderString(self: *LlvmBackend, string: []const u8) Error!void {
    if (self.strings.get(string)) |string_pointer| {
        try self.stack.append(self.allocator, .{ .value = string_pointer, .type = .string });
    } else {
        const string_pointer =
            c.LLVMBuildGlobalStringPtr(
            self.builder,
            try self.allocator.dupeZ(u8, string),
            "",
        );

        try self.strings.put(self.allocator, string, string_pointer);

        try self.stack.append(self.allocator, .{ .value = string_pointer, .type = .string });
    }
}

fn renderInt(self: *LlvmBackend, int: i128) Error!void {
    const int_type = Type.intFittingRange(int, int);

    const int_repr: c_ulonglong = @truncate(@as(u128, @bitCast(int)));

    const llvm_int_type = try self.getLlvmType(int_type);
    const llvm_int_value = c.LLVMConstInt(llvm_int_type, int_repr, @intFromBool(int < 0));

    try self.stack.append(self.allocator, .{ .value = llvm_int_value, .type = int_type });
}

fn renderFloat(self: *LlvmBackend, float: f64) Error!void {
    const float_type = Type.floatFittingRange(float, float);

    const llvm_float_type = try self.getLlvmType(float_type);
    const llvm_float_value = c.LLVMConstReal(llvm_float_type, float);

    try self.stack.append(self.allocator, .{ .value = llvm_float_value, .type = float_type });
}

fn renderBoolean(self: *LlvmBackend, boolean: bool) Error!void {
    try self.stack.append(self.allocator, .{
        .value = c.LLVMConstInt(try self.getLlvmType(.bool), @intFromBool(boolean), 0),
        .type = .bool,
    });
}

fn renderNegate(self: *LlvmBackend) Error!void {
    const rhs = self.stack.pop();

    try self.stack.append(self.allocator, .{ .value = c.LLVMBuildNeg(self.builder, rhs.value, ""), .type = rhs.type });
}

fn renderNot(self: *LlvmBackend) Error!void {
    const rhs = self.stack.pop();

    try self.stack.append(self.allocator, .{ .value = c.LLVMBuildNot(self.builder, rhs.value, ""), .type = rhs.type });
}

const BitwiseArithmeticOperation = enum {
    bit_and,
    bit_or,
    bit_xor,
};

fn renderBitwiseArithmetic(self: *LlvmBackend, comptime operation: BitwiseArithmeticOperation) Error!void {
    var rhs = self.stack.pop();
    var lhs = self.stack.pop();

    try self.binaryImplicitCast(&lhs, &rhs);

    try self.stack.append(
        self.allocator,
        .{
            .value = switch (operation) {
                .bit_and => c.LLVMBuildAnd(self.builder, lhs.value, rhs.value, ""),
                .bit_or => c.LLVMBuildOr(self.builder, lhs.value, rhs.value, ""),
                .bit_xor => c.LLVMBuildXor(self.builder, lhs.value, rhs.value, ""),
            },

            .type = lhs.type,
        },
    );
}

fn renderWrite(self: *LlvmBackend) Error!void {
    const write_pointer = self.stack.pop();

    const element_type = write_pointer.type.pointer.child_type.*;

    var write_register = self.stack.pop();

    try self.unaryImplicitCast(&write_register, element_type);

    _ = c.LLVMBuildStore(self.builder, write_register.value, write_pointer.value);
}

fn renderRead(self: *LlvmBackend) Error!void {
    const read_pointer = self.stack.pop();

    const element_type = read_pointer.type.pointer.child_type.*;

    const read_value = c.LLVMBuildLoad2(
        self.builder,
        try self.getLlvmType(element_type),
        read_pointer.value,
        "",
    );

    const read_register: Register = .{ .value = read_value, .type = element_type };

    try self.stack.append(self.allocator, read_register);
}

fn renderGetElementPtr(self: *LlvmBackend) Error!void {
    const element_index = self.stack.pop();
    var array_pointer = self.stack.pop();

    const element_type = array_pointer.type.pointer.child_type.*;

    array_pointer.type.pointer.size = .one;

    try self.stack.append(
        self.allocator,
        .{
            .value = c.LLVMBuildGEP2(
                self.builder,
                try self.getLlvmType(element_type),
                element_index.value,
                &array_pointer.value,
                1,
                "",
            ),

            .type = array_pointer.type,
        },
    );
}

fn renderGetFieldPtr(self: *LlvmBackend, field_index: u32) Error!void {
    const struct_pointer = self.stack.pop();
    const struct_type = struct_pointer.type.pointer.child_type.*;

    const field_type_on_heap = try self.allocator.create(Type);
    field_type_on_heap.* = struct_type.@"struct".fields[field_index].type;

    try self.stack.append(
        self.allocator,
        .{
            .value = c.LLVMBuildStructGEP2(
                self.builder,
                try self.getLlvmType(struct_type),
                struct_pointer.value,
                @intCast(field_index),
                "",
            ),

            .type = .{
                .pointer = .{
                    .size = .one,
                    .is_const = false,
                    .child_type = field_type_on_heap,
                },
            },
        },
    );
}

const ArithmeticOperation = enum {
    add,
    sub,
    mul,
    div,
    rem,
};

fn renderArithmetic(self: *LlvmBackend, comptime operation: ArithmeticOperation) Error!void {
    var rhs = self.stack.pop();
    var lhs = self.stack.pop();

    const usize_type: Type = .{ .int = .{ .signedness = .unsigned, .bits = self.target.ptrBitWidth() } };

    if (lhs.type == .pointer and rhs.type != .pointer) {
        rhs.value = try self.saneIntCast(rhs, usize_type);
        rhs.type = usize_type;
    } else if (rhs.type == .pointer and lhs.type != .pointer) {
        lhs.value = try self.saneIntCast(lhs, usize_type);
        lhs.type = usize_type;
    } else {
        try self.binaryImplicitCast(&lhs, &rhs);
    }

    if (lhs.type == .int or lhs.type == .pointer) {
        try self.stack.append(
            self.allocator,
            .{
                .value = switch (operation) {
                    .add => c.LLVMBuildAdd(self.builder, lhs.value, rhs.value, ""),
                    .sub => c.LLVMBuildSub(self.builder, lhs.value, rhs.value, ""),
                    .mul => c.LLVMBuildMul(self.builder, lhs.value, rhs.value, ""),
                    .div => if (lhs.type.canBeNegative())
                        c.LLVMBuildSDiv(self.builder, lhs.value, rhs.value, "")
                    else
                        c.LLVMBuildUDiv(self.builder, lhs.value, rhs.value, ""),
                    .rem => if (lhs.type.canBeNegative())
                        c.LLVMBuildSRem(self.builder, lhs.value, rhs.value, "")
                    else
                        c.LLVMBuildURem(self.builder, lhs.value, rhs.value, ""),
                },

                .type = lhs.type,
            },
        );
    } else {
        try self.stack.append(
            self.allocator,
            .{
                .value = switch (operation) {
                    .add => c.LLVMBuildFAdd(self.builder, lhs.value, rhs.value, ""),
                    .sub => c.LLVMBuildFSub(self.builder, lhs.value, rhs.value, ""),
                    .mul => c.LLVMBuildFMul(self.builder, lhs.value, rhs.value, ""),
                    .div => c.LLVMBuildFDiv(self.builder, lhs.value, rhs.value, ""),
                    .rem => c.LLVMBuildFRem(self.builder, lhs.value, rhs.value, ""),
                },

                .type = lhs.type,
            },
        );
    }
}

fn renderComparison(self: *LlvmBackend, operation: Air.Instruction.Cmp) Error!void {
    var rhs = self.stack.pop();
    var lhs = self.stack.pop();

    try self.binaryImplicitCast(&lhs, &rhs);

    if (lhs.type == .int) {
        try self.stack.append(
            self.allocator,
            .{
                .value = switch (operation) {
                    .lt => c.LLVMBuildICmp(self.builder, if (lhs.type.canBeNegative()) c.LLVMIntSLT else c.LLVMIntULT, lhs.value, rhs.value, ""),
                    .gt => c.LLVMBuildICmp(self.builder, if (lhs.type.canBeNegative()) c.LLVMIntSGT else c.LLVMIntUGT, lhs.value, rhs.value, ""),
                    .eql => c.LLVMBuildICmp(self.builder, c.LLVMIntEQ, lhs.value, rhs.value, ""),
                },

                .type = lhs.type,
            },
        );
    } else {
        try self.stack.append(
            self.allocator,
            .{
                .value = switch (operation) {
                    .lt => c.LLVMBuildFCmp(self.builder, c.LLVMRealOLT, lhs.value, rhs.value, ""),
                    .gt => c.LLVMBuildFCmp(self.builder, c.LLVMRealOGT, lhs.value, rhs.value, ""),
                    .eql => c.LLVMBuildFCmp(self.builder, c.LLVMRealOEQ, lhs.value, rhs.value, ""),
                },

                .type = lhs.type,
            },
        );
    }
}

const BitwiseShiftDirection = enum {
    left,
    right,
};

fn renderBitwiseShift(self: *LlvmBackend, comptime direction: BitwiseShiftDirection) Error!void {
    const rhs = self.stack.pop();
    const lhs = self.stack.pop();

    try self.stack.append(
        self.allocator,
        .{
            .value = switch (direction) {
                .left => c.LLVMBuildShl(self.builder, lhs.value, rhs.value, ""),
                .right => c.LLVMBuildAShr(self.builder, lhs.value, rhs.value, ""),
            },

            .type = lhs.type,
        },
    );
}

fn renderCast(self: *LlvmBackend, cast_to: Type) Error!void {
    const lhs = self.stack.pop();

    const cast_value = lhs.value;
    const cast_from = lhs.type;

    const llvm_cast_to = try self.getLlvmType(cast_to);

    try self.stack.append(
        self.allocator,
        .{
            .value = switch (cast_from) {
                .int => switch (cast_to) {
                    .int, .bool => try self.saneIntCast(lhs, cast_to),

                    .float => if (cast_from.canBeNegative())
                        c.LLVMBuildSIToFP(self.builder, cast_value, llvm_cast_to, "")
                    else
                        c.LLVMBuildUIToFP(self.builder, cast_value, llvm_cast_to, ""),

                    .pointer => c.LLVMBuildIntToPtr(self.builder, cast_value, llvm_cast_to, ""),

                    else => unreachable,
                },

                .float => switch (cast_to) {
                    .int => if (cast_to.canBeNegative())
                        c.LLVMBuildFPToSI(self.builder, cast_value, llvm_cast_to, "")
                    else
                        c.LLVMBuildFPToUI(self.builder, cast_value, llvm_cast_to, ""),

                    .float => c.LLVMBuildFPCast(self.builder, cast_value, llvm_cast_to, ""),

                    else => unreachable,
                },

                .pointer => switch (cast_to) {
                    .int => c.LLVMBuildPtrToInt(self.builder, cast_value, llvm_cast_to, ""),
                    .pointer => c.LLVMBuildPointerCast(self.builder, cast_value, llvm_cast_to, ""),

                    else => unreachable,
                },

                .bool => c.LLVMBuildZExt(self.builder, cast_value, llvm_cast_to, ""),

                .void => unreachable,
                .function => unreachable,
                .@"struct" => unreachable,
            },

            .type = cast_to,
        },
    );
}

fn renderAssembly(self: *LlvmBackend, assembly: Air.Instruction.Assembly) Error!void {
    const assembly_inputs = try self.allocator.alloc(c.LLVMValueRef, assembly.input_constraints.len);

    var assembly_constraints: std.ArrayListUnmanaged(u8) = .{};

    if (assembly.output_constraint) |output_constraint| {
        try assembly_constraints.appendSlice(self.allocator, output_constraint.register);
        if (assembly.input_constraints.len != 0) try assembly_constraints.append(self.allocator, ',');
    }

    for (assembly.input_constraints, 0..) |register, i| {
        assembly_inputs[i] = self.stack.pop().value;

        try assembly_constraints.appendSlice(self.allocator, register);
        if (assembly.clobbers.len != 0 or i != assembly.input_constraints.len - 1) try assembly_constraints.append(self.allocator, ',');
    }

    for (assembly.clobbers, 0..) |clobber, i| {
        try assembly_constraints.writer(self.allocator).print("~{{{s}}}", .{clobber});
        if (i != assembly.clobbers.len - 1) try assembly_constraints.append(self.allocator, ',');
    }

    if (self.target.cpu.arch.isX86()) {
        if (assembly_constraints.items.len != 0) try assembly_constraints.append(self.allocator, ',');
        try assembly_constraints.appendSlice(self.allocator, "~{dirflag},~{fpsr},~{flags}");
    }

    const assembly_parameter_types = try self.allocator.alloc(c.LLVMTypeRef, assembly.input_constraints.len);

    for (assembly_inputs, 0..) |input, i| {
        assembly_parameter_types[i] = c.LLVMTypeOf(input);
    }

    const assmebly_return_type = try self.getLlvmType(if (assembly.output_constraint) |output_constraint| output_constraint.type else .void);

    const assembly_function_type = c.LLVMFunctionType(assmebly_return_type, assembly_parameter_types.ptr, @intCast(assembly_parameter_types.len), 0);

    const assembly_function = c.LLVMGetInlineAsm(
        assembly_function_type,
        try self.allocator.dupeZ(u8, assembly.content),
        assembly.content.len,
        assembly_constraints.items.ptr,
        assembly_constraints.items.len,
        1,
        0,
        c.LLVMInlineAsmDialectATT,
        0,
    );

    const assembly_output = c.LLVMBuildCall2(
        self.builder,
        assembly_function_type,
        assembly_function,
        assembly_inputs.ptr,
        @intCast(assembly_inputs.len),
        "",
    );

    if (assembly.output_constraint) |assembly_output_constraint| {
        try self.stack.append(self.allocator, .{ .value = assembly_output, .type = assembly_output_constraint.type });
    }
}

fn renderCall(self: *LlvmBackend, arguments_count: usize) Error!void {
    const function_pointer = self.stack.pop();

    const function_type = function_pointer.type.pointer.child_type.*;
    const function_return_type = function_type.function.return_type.*;

    const arguments = try self.allocator.alloc(c.LLVMValueRef, arguments_count);

    for (0..arguments_count) |i| {
        var argument = self.stack.pop();

        if (i < function_type.function.parameter_types.len) {
            try self.unaryImplicitCast(&argument, function_type.function.parameter_types[i]);
        }

        arguments[i] = argument.value;
    }

    const call = c.LLVMBuildCall2(
        self.builder,
        try self.getLlvmType(function_type),
        function_pointer.value,
        arguments.ptr,
        @intCast(arguments_count),
        "",
    );

    if (function_return_type != .void) {
        try self.stack.append(self.allocator, .{ .value = call, .type = function_return_type });
    }
}

fn renderFunction(self: *LlvmBackend, symbol: Symbol, remainder_instructions: []const Air.Instruction) Error!void {
    // Should already be in scope because it is hoisted in the first pass in `render`
    const function_pointer = self.scope.get(symbol.name.buffer).?.pointer;

    self.function_value = function_pointer;
    self.function_type = symbol.type.pointer.child_type.*;

    var scope_depth: usize = 0;

    for (remainder_instructions) |air_instruction| {
        switch (air_instruction) {
            .block => |block| try self.basic_blocks.put(self.allocator, block.id, c.LLVMAppendBasicBlock(function_pointer, "")),
            .start_scope => scope_depth += 1,
            .end_scope => {
                scope_depth -= 1;
                if (scope_depth == 0) break;
            },

            else => {},
        }
    }
}

fn renderParameters(self: *LlvmBackend, symbols: []const Symbol) Error!void {
    for (symbols, 0..) |symbol, i| {
        const parameter_pointer = c.LLVMBuildAlloca(self.builder, try self.getLlvmType(symbol.type), "");

        _ = c.LLVMBuildStore(self.builder, c.LLVMGetParam(self.function_value, @intCast(i)), parameter_pointer);

        try self.scope.put(
            self.allocator,
            symbol.name.buffer,
            .{
                .pointer = parameter_pointer,
                .type = symbol.type,
                .linkage = symbol.linkage,
            },
        );
    }
}

fn renderVariable(self: *LlvmBackend, symbol: Symbol) Error!void {
    const llvm_type = try self.getLlvmType(symbol.type);

    if (self.scope.get(symbol.name.buffer)) |variable| {
        if (variable.linkage == .global) {
            var register = self.stack.popOrNull() orelse Register{ .value = c.LLVMGetUndef(llvm_type), .type = symbol.type };

            try self.unaryImplicitCast(&register, symbol.type);

            _ = c.LLVMSetInitializer(variable.pointer, register.value);
        }
    } else {
        const variable_pointer = switch (symbol.linkage) {
            .global => blk: {
                const global_variable_pointer = c.LLVMAddGlobal(
                    self.module,
                    llvm_type,
                    try self.allocator.dupeZ(u8, symbol.name.buffer),
                );

                var register = self.stack.popOrNull() orelse Register{ .value = c.LLVMGetUndef(llvm_type), .type = symbol.type };

                try self.unaryImplicitCast(&register, symbol.type);

                _ = c.LLVMSetInitializer(global_variable_pointer, register.value);

                break :blk global_variable_pointer;
            },

            .local => blk: {
                const current_block = c.LLVMGetInsertBlock(self.builder);
                const first_block = c.LLVMGetFirstBasicBlock(self.function_value);
                const first_instruction = c.LLVMGetFirstInstruction(first_block);

                if (first_instruction != null) {
                    c.LLVMPositionBuilderBefore(self.builder, first_instruction);
                } else {
                    c.LLVMPositionBuilderAtEnd(self.builder, first_block);
                }

                const local_variable_pointer = c.LLVMBuildAlloca(self.builder, llvm_type, "");

                c.LLVMPositionBuilderAtEnd(self.builder, current_block);

                break :blk local_variable_pointer;
            },

            .external => unreachable,
        };

        try self.scope.put(
            self.allocator,
            symbol.name.buffer,
            .{
                .pointer = variable_pointer,
                .type = symbol.type,
                .linkage = symbol.linkage,
            },
        );
    }
}

fn renderExternal(self: *LlvmBackend, symbol: Symbol) Error!void {
    if (self.scope.get(symbol.name.buffer) != null) return;

    const pointer = if (symbol.type.getFunction() != null)
        c.LLVMAddFunction(
            self.module,
            try self.allocator.dupeZ(u8, symbol.name.buffer),
            try self.getLlvmType(symbol.type.pointer.child_type.*),
        )
    else
        c.LLVMAddGlobal(
            self.module,
            try self.getLlvmType(symbol.type),
            try self.allocator.dupeZ(u8, symbol.name.buffer),
        );

    c.LLVMSetLinkage(pointer, c.LLVMExternalLinkage);
    c.LLVMSetVisibility(pointer, c.LLVMDefaultVisibility);

    try self.scope.put(
        self.allocator,
        symbol.name.buffer,
        .{
            .pointer = pointer,
            .type = symbol.type,
            .linkage = .global,
        },
    );
}

fn renderSet(self: *LlvmBackend, name: []const u8) Error!void {
    const variable = self.scope.get(name).?;

    var register = self.stack.pop();

    try self.unaryImplicitCast(&register, variable.type);

    _ = c.LLVMBuildStore(self.builder, register.value, variable.pointer);
}

fn renderGet(self: *LlvmBackend, name: []const u8) Error!void {
    const variable = self.scope.get(name).?;

    if (variable.type.getFunction() != null) {
        try self.stack.append(self.allocator, .{ .value = variable.pointer, .type = variable.type });
    } else {
        const variable_load = c.LLVMBuildLoad2(
            self.builder,
            try self.getLlvmType(variable.type),
            variable.pointer,
            "",
        );

        try self.stack.append(self.allocator, .{ .value = variable_load, .type = variable.type });
    }
}

fn renderGetPtr(self: *LlvmBackend, name: []const u8) Error!void {
    const variable = self.scope.getPtr(name).?;

    try self.stack.append(
        self.allocator,
        .{
            .value = variable.pointer,
            .type = .{
                .pointer = .{
                    .size = .one,
                    .is_const = false,
                    .child_type = &variable.type,
                },
            },
        },
    );
}

fn renderBlock(self: *LlvmBackend, block: Air.Instruction.Block) Error!void {
    c.LLVMPositionBuilderAtEnd(self.builder, self.basic_blocks.get(block.id).?);
}

fn renderBr(self: *LlvmBackend, br: Air.Instruction.Br) Error!void {
    _ = c.LLVMBuildBr(self.builder, self.basic_blocks.get(br.id).?);
}

fn renderCondBr(self: *LlvmBackend, cond_br: Air.Instruction.CondBr) Error!void {
    const condition_value = self.stack.pop().value;

    const true_basic_block = self.basic_blocks.get(cond_br.true_id).?;
    const false_basic_block = self.basic_blocks.get(cond_br.false_id).?;

    _ = c.LLVMBuildCondBr(
        self.builder,
        condition_value,
        true_basic_block,
        false_basic_block,
    );
}

fn modifyScope(self: *LlvmBackend, start: bool) Error!void {
    if (start) {
        const local_scope = try self.scope_stack.addOne(self.allocator);
        local_scope.* = .{ .maybe_parent = self.scope };
        self.scope = local_scope;
    } else {
        self.scope.deinit(self.allocator);
        self.scope = self.scope.maybe_parent.?;
        _ = self.scope_stack.pop();
    }
}

fn renderReturn(self: *LlvmBackend, with_value: bool) Error!void {
    if (with_value) {
        var return_register = self.stack.pop();
        const return_type = self.function_type.function.return_type.*;

        try self.unaryImplicitCast(&return_register, return_type);

        _ = c.LLVMBuildRet(self.builder, return_register.value);
    } else {
        _ = c.LLVMBuildRetVoid(self.builder);
    }
}
