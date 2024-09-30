const std = @import("std");
const root = @import("root");

const Ast = @import("Ast.zig");
const Assembly = @import("Assembly.zig");
const Hir = @import("Hir.zig");
const Lir = @import("Lir.zig");
const Sema = @import("Sema.zig");
const Cli = root.Cli;

const Compilation = @This();

allocator: std.mem.Allocator,

env: Environment,

pub const Environment = struct {
    cerium_lib_dir: std.fs.Dir,
    source_file_path: []const u8,
    target: std.Target,

    pub inline fn openCeriumLibrary() !std.fs.Dir {
        var self_exe_dir_path_buf: [std.fs.max_path_bytes]u8 = undefined;

        const self_exe_dir_path = try std.fs.selfExeDirPath(&self_exe_dir_path_buf);

        const self_exe_dir = try std.fs.openDirAbsolute(self_exe_dir_path, .{});

        var dir = self_exe_dir;

        var opened = false;

        while (!opened) {
            opened = true;

            dir = dir.openDir("lib/cerium", .{}) catch |err| switch (err) {
                error.FileNotFound => blk: {
                    break :blk dir.openDir("lib", .{}) catch |another_err| switch (another_err) {
                        error.FileNotFound => {
                            opened = false;

                            break :blk try dir.openDir("..", .{});
                        },

                        else => return err,
                    };
                },

                else => return err,
            };
        }

        return dir;
    }
};

pub fn init(allocator: std.mem.Allocator, env: Environment) Compilation {
    return Compilation{
        .allocator = allocator,
        .env = env,
    };
}

pub fn parse(self: Compilation, input: [:0]const u8) ?Ast {
    var ast_parser = Ast.Parser.init(self.allocator, self.env, input) catch |err| {
        std.debug.print("{s}\n", .{Cli.errorDescription(err)});

        return null;
    };

    defer ast_parser.deinit();

    const ast = ast_parser.parse() catch |err| switch (err) {
        error.OutOfMemory => {
            std.debug.print("{s}\n", .{Cli.errorDescription(err)});

            return null;
        },

        else => {
            std.debug.print("{s}:{}:{}: {s}\n", .{ self.env.source_file_path, ast_parser.error_info.?.source_loc.line, ast_parser.error_info.?.source_loc.column, ast_parser.error_info.?.message });

            return null;
        },
    };

    return ast;
}

pub fn generateHir(self: Compilation, ast: Ast) ?Hir {
    var hir_generator = Hir.Generator.init(self.allocator);

    hir_generator.generate(ast) catch |err| switch (err) {
        error.OutOfMemory => {
            std.debug.print("{s}\n", .{Cli.errorDescription(err)});

            return null;
        },

        else => {
            std.debug.print("{s}:{}:{}: {s}\n", .{ self.env.source_file_path, hir_generator.error_info.?.source_loc.line, hir_generator.error_info.?.source_loc.column, hir_generator.error_info.?.message });

            return null;
        },
    };

    return hir_generator.hir;
}

pub fn analyzeSemantics(self: Compilation, hir: Hir) ?Lir {
    var sema = Sema.init(self.allocator, self.env);
    defer sema.deinit();

    sema.analyze(hir) catch |err| switch (err) {
        error.OutOfMemory => {
            std.debug.print("{s}\n", .{Cli.errorDescription(err)});

            return null;
        },

        else => {
            std.debug.print("{s}:{}:{}: {s}\n", .{ self.env.source_file_path, sema.error_info.?.source_loc.line, sema.error_info.?.source_loc.column, sema.error_info.?.message });

            return null;
        },
    };

    return sema.lir;
}

pub fn compileLir(self: Compilation, input: [:0]const u8) ?Lir {
    // TODO: Ast is leaking memory..
    const ast = self.parse(input) orelse return null;

    // TODO: Hir is leaking memory..
    const hir = self.generateHir(ast) orelse return null;

    // TODO: Some of the strucures in Lir depend on Hir memory allocated data, we can not free Hir memory here.
    // find a way to free Hir memory when we are done with it or do not depend on it anymore.
    const lir = self.analyzeSemantics(hir) orelse return null;

    return lir;
}

pub fn concatLir(self: Compilation, lirs: []const Lir) std.mem.Allocator.Error!Lir {
    var concatenated_lir: Lir = .{};

    for (lirs) |lir| {
        try concatenated_lir.global.ensureUnusedCapacity(self.allocator, lir.global.count());

        for (lir.global.keys(), lir.global.values()) |lir_block_name, lir_block| {
            concatenated_lir.global.putAssumeCapacity(lir_block_name, lir_block);
        }

        try concatenated_lir.external.ensureUnusedCapacity(self.allocator, lir.external.count());

        for (lir.external.keys(), lir.external.values()) |lir_type_name, lir_type| {
            concatenated_lir.external.putAssumeCapacity(lir_type_name, lir_type);
        }

        try concatenated_lir.functions.ensureUnusedCapacity(self.allocator, lir.functions.count());

        for (lir.functions.keys(), lir.functions.values()) |lir_function_name, lir_function| {
            concatenated_lir.functions.putAssumeCapacity(lir_function_name, lir_function);
        }
    }

    return concatenated_lir;
}

pub fn renderAssembly(self: Compilation, lir: Lir) ?[]u8 {
    return switch (self.env.target.cpu.arch) {
        .x86_64 => blk: {
            var backend = Assembly.x86_64.init(self.allocator, lir);
            defer backend.deinit();

            backend.render() catch |err| {
                std.debug.print("{s}\n", .{Cli.errorDescription(err)});

                return null;
            };

            break :blk backend.finalize() catch null;
        },

        else => {
            std.debug.print("{s} is not supported yet", .{self.env.target.cpu.arch.genericName()});

            return null;
        },
    };
}
