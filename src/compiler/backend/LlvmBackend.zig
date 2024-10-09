const std = @import("std");

const c = @cImport({
    @cInclude("llvm-c/Core.h");
    @cInclude("llvm-c/Target.h");
    @cInclude("llvm-c/TargetMachine.h");
});

const Compilation = @import("../Compilation.zig");
const Lir = @import("../Lir.zig");
const Symbol = @import("../Symbol.zig");
const Scope = Symbol.Scope;
const Type = Symbol.Type;

const LlvmBackend = @This();

allocator: std.mem.Allocator,

target: std.Target,

lir: Lir,
counter: usize = 0,

context: c.LLVMContextRef,
module: c.LLVMModuleRef,
builder: c.LLVMBuilderRef,

maybe_function: ?c.LLVMValueRef = null,
basic_blocks: std.AutoArrayHashMapUnmanaged(u32, c.LLVMBasicBlockRef) = .{},

strings: std.StringHashMapUnmanaged(c.LLVMValueRef) = .{},

stack: std.ArrayListUnmanaged(c.LLVMValueRef) = .{},

scope: *Scope(Variable) = undefined,
scope_stack: std.ArrayListUnmanaged(Scope(Variable)) = .{},

pub const Error = std.mem.Allocator.Error;

pub const Variable = struct {
    pointer: c.LLVMValueRef,
    type: Type,
    linkage: Symbol.Linkage,
};

pub fn init(allocator: std.mem.Allocator, target: std.Target, lir: Lir) LlvmBackend {
    const context = c.LLVMContextCreate();
    const module = c.LLVMModuleCreateWithNameInContext("module", context);
    const builder = c.LLVMCreateBuilderInContext(context);

    return LlvmBackend{
        .allocator = allocator,
        .target = target,
        .lir = lir,
        .context = context,
        .module = module,
        .builder = builder,
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

pub fn emit(self: *LlvmBackend, output_file_path: [:0]const u8, output_kind: Compilation.OutputKind) Error!void {
    c.LLVMInitializeAllTargetInfos();
    c.LLVMInitializeAllTargets();
    c.LLVMInitializeAllTargetMCs();
    c.LLVMInitializeAllAsmParsers();
    c.LLVMInitializeAllAsmPrinters();

    c.LLVMDumpModule(self.module);

    const target_triple = try std.fmt.allocPrintZ(self.allocator, "{s}-{s}-{s}", .{
        @tagName(self.target.cpu.arch),
        @tagName(self.target.os.tag),
        @tagName(self.target.abi),
    });

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
        c.LLVMRelocDefault,
        c.LLVMCodeModelDefault,
    );

    _ = c.LLVMTargetMachineEmitToFile(
        target_machine,
        self.module,
        output_file_path,
        if (output_kind == .object) c.LLVMObjectFile else c.LLVMAssemblyFile,
        null,
    );
}

fn getLlvmType(self: *LlvmBackend, @"type": Type) Error!c.LLVMTypeRef {
    return switch (@"type") {
        .void => c.LLVMVoidTypeInContext(self.context),
        .bool => c.LLVMIntTypeInContext(self.context, 1),
        .int => |int| c.LLVMIntTypeInContext(self.context, int.bits),
        .float => |float| if (float.bits == 32) c.LLVMFloatTypeInContext(self.context) else c.LLVMDoubleTypeInContext(self.context),
        .pointer => |pointer| c.LLVMPointerType(try self.getLlvmType(pointer.child_type.*), 1),

        .function => |function| blk: {
            const parameter_types = try self.allocator.alloc(c.LLVMTypeRef, function.parameter_types.len);

            for (function.parameter_types, 0..) |parameter, i| {
                parameter_types[i] = try self.getLlvmType(parameter);
            }

            const return_type = try self.getLlvmType(function.return_type.*);

            const function_type = c.LLVMFunctionType(return_type, parameter_types.ptr, @intCast(parameter_types.len), 0);

            break :blk function_type;
        },

        else => unreachable,
    };
}

pub fn render(self: *LlvmBackend) Error!void {
    const global_scope = try self.scope_stack.addOne(self.allocator);
    global_scope.* = .{};
    self.scope = global_scope;

    for (self.lir.instructions.items) |lir_instruction| {
        try self.renderInstruction(lir_instruction);

        self.counter += 1;
    }
}

fn renderInstruction(self: *LlvmBackend, lir_instruction: Lir.Instruction) Error!void {
    switch (lir_instruction) {
        .duplicate => try self.stack.append(self.allocator, self.stack.getLast()),
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
        .read => |element_type| try self.renderRead(element_type),

        .add => try self.renderArithmetic(.add),
        .sub => try self.renderArithmetic(.sub),
        .mul => try self.renderArithmetic(.mul),
        .fdiv => try self.renderArithmetic(.fdiv),
        .sdiv => try self.renderArithmetic(.sdiv),
        .udiv => try self.renderArithmetic(.udiv),

        .icmp => |operation| try self.renderIntComparison(operation),
        .fcmp => |operation| try self.renderFloatComparison(operation),

        .shl => try self.renderBitwiseShift(.left),
        .shr => try self.renderBitwiseShift(.right),

        .assembly => |assembly| try self.renderAssembly(assembly),

        .call => |function_type| try self.renderCall(function_type),

        .function => |function| try self.renderFunction(function),

        .parameters => |parameters| try self.renderParameters(parameters),

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
        try self.stack.append(self.allocator, string_pointer);
    } else {
        const string_pointer =
            c.LLVMBuildGlobalStringPtr(
            self.builder,
            try self.allocator.dupeZ(u8, string),
            "",
        );

        try self.strings.put(self.allocator, string, string_pointer);

        try self.stack.append(self.allocator, string_pointer);
    }
}

fn renderInt(self: *LlvmBackend, int: i128) Error!void {
    const bits: c_uint = @intFromFloat(@ceil(@log2(@as(f64, @floatFromInt(@abs(int) + 1)))));
    const int_repr: c_ulonglong = @truncate(@as(u128, @bitCast(int)));
    const int_type = c.LLVMIntTypeInContext(self.context, std.math.clamp(bits, 8, 64));

    try self.stack.append(self.allocator, c.LLVMConstInt(int_type, int_repr, @intFromBool(int < 0)));
}

fn renderFloat(self: *LlvmBackend, float: f64) Error!void {
    const bits: c_uint = @intFromFloat(@ceil(@log2(float + 1)));
    const float_type = if (bits <= 32) c.LLVMFloatTypeInContext(self.context) else c.LLVMDoubleTypeInContext(self.context);

    try self.stack.append(self.allocator, c.LLVMConstReal(float_type, float));
}

fn renderBoolean(self: *LlvmBackend, boolean: bool) Error!void {
    try self.stack.append(self.allocator, c.LLVMConstInt(try self.getLlvmType(.bool), @intFromBool(boolean), 0));
}

fn renderNegate(self: *LlvmBackend) Error!void {
    const rhs = self.stack.pop();

    try self.stack.append(self.allocator, c.LLVMBuildNeg(self.builder, rhs, ""));
}

fn renderNot(self: *LlvmBackend) Error!void {
    const rhs = self.stack.pop();

    try self.stack.append(self.allocator, c.LLVMBuildNot(self.builder, rhs, ""));
}

const BitwiseArithmeticOperation = enum {
    bit_and,
    bit_or,
    bit_xor,
};

fn renderBitwiseArithmetic(self: *LlvmBackend, comptime operation: BitwiseArithmeticOperation) Error!void {
    const rhs = self.stack.pop();
    const lhs = self.stack.pop();

    try self.stack.append(
        self.allocator,
        switch (operation) {
            .bit_and => c.LLVMBuildAnd(self.builder, lhs, rhs, ""),
            .bit_or => c.LLVMBuildOr(self.builder, lhs, rhs, ""),
            .bit_xor => c.LLVMBuildXor(self.builder, lhs, rhs, ""),
        },
    );
}

fn renderWrite(self: *LlvmBackend) Error!void {
    const write_pointer = self.stack.pop();
    const write_value = self.stack.pop();

    _ = c.LLVMBuildStore(
        self.builder,
        write_value,
        write_pointer,
    );
}

fn renderRead(self: *LlvmBackend, element_type: Type) Error!void {
    const read_pointer = self.stack.pop();

    const read_value = c.LLVMBuildLoad2(
        self.builder,
        try self.getLlvmType(element_type),
        read_pointer,
        "",
    );

    try self.stack.append(
        self.allocator,
        read_value,
    );
}

const ArithmeticOperation = enum {
    add,
    sub,
    mul,
    fdiv,
    udiv,
    sdiv,
};

fn renderArithmetic(self: *LlvmBackend, comptime operation: ArithmeticOperation) Error!void {
    const rhs = self.stack.pop();
    const lhs = self.stack.pop();

    const lhs_type_kind = c.LLVMGetTypeKind(c.LLVMTypeOf(lhs));

    if (lhs_type_kind == c.LLVMIntegerTypeKind or lhs_type_kind == c.LLVMPointerTypeKind) {
        try self.stack.append(
            self.allocator,
            switch (operation) {
                .add => c.LLVMBuildAdd(self.builder, lhs, rhs, ""),
                .sub => c.LLVMBuildSub(self.builder, lhs, rhs, ""),
                .mul => c.LLVMBuildMul(self.builder, lhs, rhs, ""),
                .fdiv => unreachable,
                .udiv => c.LLVMBuildUDiv(self.builder, lhs, rhs, ""),
                .sdiv => c.LLVMBuildSDiv(self.builder, lhs, rhs, ""),
            },
        );
    } else {
        try self.stack.append(
            self.allocator,
            switch (operation) {
                .add => c.LLVMBuildFAdd(self.builder, lhs, rhs, ""),
                .sub => c.LLVMBuildFSub(self.builder, lhs, rhs, ""),
                .mul => c.LLVMBuildFMul(self.builder, lhs, rhs, ""),
                .fdiv => c.LLVMBuildFDiv(self.builder, lhs, rhs, ""),
                .udiv => unreachable,
                .sdiv => unreachable,
            },
        );
    }
}

fn renderIntComparison(self: *LlvmBackend, operation: Lir.Instruction.ICmp) Error!void {
    const rhs = self.stack.pop();
    const lhs = self.stack.pop();

    try self.stack.append(
        self.allocator,
        switch (operation) {
            .slt => c.LLVMBuildICmp(self.builder, c.LLVMIntSLT, lhs, rhs, ""),
            .sgt => c.LLVMBuildICmp(self.builder, c.LLVMIntSGT, lhs, rhs, ""),
            .ult => c.LLVMBuildICmp(self.builder, c.LLVMIntULT, lhs, rhs, ""),
            .ugt => c.LLVMBuildICmp(self.builder, c.LLVMIntUGT, lhs, rhs, ""),
            .eql => c.LLVMBuildICmp(self.builder, c.LLVMIntEQ, lhs, rhs, ""),
        },
    );
}

fn renderFloatComparison(self: *LlvmBackend, operation: Lir.Instruction.FCmp) Error!void {
    const rhs = self.stack.pop();
    const lhs = self.stack.pop();

    try self.stack.append(
        self.allocator,
        switch (operation) {
            .lt => c.LLVMBuildFCmp(self.builder, c.LLVMRealOLT, lhs, rhs, ""),
            .gt => c.LLVMBuildFCmp(self.builder, c.LLVMRealOGT, lhs, rhs, ""),
            .eql => c.LLVMBuildFCmp(self.builder, c.LLVMRealOEQ, lhs, rhs, ""),
        },
    );
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
        switch (direction) {
            .left => c.LLVMBuildShl(self.builder, lhs, rhs, ""),
            .right => c.LLVMBuildAShr(self.builder, lhs, rhs, ""),
        },
    );
}

fn renderAssembly(self: *LlvmBackend, assembly: Lir.Instruction.Assembly) Error!void {
    const assembly_inputs = try self.allocator.alloc(c.LLVMValueRef, assembly.input_constraints.len);

    var assembly_constraints: std.ArrayListUnmanaged(u8) = .{};

    if (assembly.output_constraint) |output_constraint| {
        try assembly_constraints.appendSlice(self.allocator, output_constraint.register);
        try assembly_constraints.append(self.allocator, ',');
    }

    for (assembly.input_constraints, 0..) |register, i| {
        assembly_inputs[i] = self.stack.pop();

        try assembly_constraints.appendSlice(self.allocator, register);
        if (i != assembly.input_constraints.len - 1) try assembly_constraints.append(self.allocator, ',');
    }

    if (self.target.cpu.arch.isX86()) {
        try assembly_constraints.appendSlice(self.allocator, ",~{dirflag},~{fpsr},~{flags}");
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

    try self.stack.append(self.allocator, assembly_output);
}

fn renderCall(self: *LlvmBackend, function_type: Type.Function) Error!void {
    const function_pointer = self.stack.pop();

    const call_arguments = try self.allocator.alloc(c.LLVMValueRef, function_type.parameter_types.len);

    for (0..function_type.parameter_types.len) |i| {
        const argument = self.stack.pop();

        call_arguments[i] = argument;
    }

    const call = c.LLVMBuildCall2(
        self.builder,
        try self.getLlvmType(.{ .function = function_type }),
        function_pointer,
        call_arguments.ptr,
        @intCast(call_arguments.len),
        "",
    );

    try self.stack.append(self.allocator, call);
}

fn renderFunction(self: *LlvmBackend, function: Lir.Instruction.Function) Error!void {
    const function_pointer = if (self.scope.get(function.name)) |function_variable|
        function_variable.pointer
    else blk: {
        const function_pointer = c.LLVMAddFunction(
            self.module,
            try self.allocator.dupeZ(u8, function.name),
            try self.getLlvmType(function.type.pointer.child_type.*),
        );

        try self.scope.put(
            self.allocator,
            function.name,
            .{
                .pointer = function_pointer,
                .type = function.type,
                .linkage = .global,
            },
        );

        break :blk function_pointer;
    };

    self.maybe_function = function_pointer;

    var scope_depth: usize = 0;

    for (self.lir.instructions.items[self.counter..]) |lir_instruction| {
        switch (lir_instruction) {
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

fn renderParameters(self: *LlvmBackend, parameters: []const Symbol) Error!void {
    for (parameters, 0..) |parameter, i| {
        const parameter_pointer = c.LLVMBuildAlloca(self.builder, try self.getLlvmType(parameter.type), "");

        _ = c.LLVMBuildStore(self.builder, c.LLVMGetParam(self.maybe_function.?, @intCast(i)), parameter_pointer);

        try self.scope.put(
            self.allocator,
            parameter.name.buffer,
            .{
                .pointer = parameter_pointer,
                .type = parameter.type,
                .linkage = .local,
            },
        );
    }
}

fn renderVariable(self: *LlvmBackend, symbol: Symbol) Error!void {
    const llvm_type = try self.getLlvmType(symbol.type);

    if (self.scope.get(symbol.name.buffer)) |variable| {
        if (variable.linkage == .global) {
            _ = c.LLVMSetInitializer(variable.pointer, self.stack.pop());
        }
    } else {
        const variable_pointer = switch (symbol.linkage) {
            .global => blk: {
                const global_variable_pointer = c.LLVMAddGlobal(
                    self.module,
                    llvm_type,
                    try self.allocator.dupeZ(u8, symbol.name.buffer),
                );

                _ = c.LLVMSetInitializer(global_variable_pointer, self.stack.pop());

                break :blk global_variable_pointer;
            },

            .local => c.LLVMBuildAlloca(self.builder, llvm_type, ""),
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

    _ = c.LLVMBuildStore(
        self.builder,
        self.stack.pop(),
        variable.pointer,
    );
}

fn renderGet(self: *LlvmBackend, name: []const u8) Error!void {
    const variable = self.scope.get(name).?;

    const variable_load = c.LLVMBuildLoad2(
        self.builder,
        try self.getLlvmType(variable.type),
        variable.pointer,
        "",
    );

    try self.stack.append(
        self.allocator,
        variable_load,
    );
}

fn renderGetPtr(self: *LlvmBackend, name: []const u8) Error!void {
    const variable = self.scope.get(name).?;

    try self.stack.append(
        self.allocator,
        variable.pointer,
    );
}

fn renderBlock(self: *LlvmBackend, block: Lir.Instruction.Block) Error!void {
    c.LLVMPositionBuilderAtEnd(self.builder, self.basic_blocks.get(block.id).?);
}

fn renderBr(self: *LlvmBackend, br: Lir.Instruction.Br) Error!void {
    _ = c.LLVMBuildBr(self.builder, self.basic_blocks.get(br.id).?);
}

fn renderCondBr(self: *LlvmBackend, cond_br: Lir.Instruction.CondBr) Error!void {
    const condition = self.stack.pop();

    const true_basic_block = self.basic_blocks.get(cond_br.true_id).?;
    const false_basic_block = self.basic_blocks.get(cond_br.false_id).?;

    _ = c.LLVMBuildCondBr(
        self.builder,
        condition,
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
        self.scope.clearAndFree(self.allocator);
        self.scope = self.scope.maybe_parent.?;
        _ = self.scope_stack.pop();
    }
}

fn renderReturn(self: *LlvmBackend, with_value: bool) Error!void {
    if (with_value) {
        const return_value = self.stack.pop();

        _ = c.LLVMBuildRet(self.builder, return_value);
    } else {
        _ = c.LLVMBuildRetVoid(self.builder);
    }
}
