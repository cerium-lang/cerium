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
pipeline: Pipeline = .{},

pub const Environment = struct {
    cerium_lib_dir: std.fs.Dir,
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

pub const Pipeline = struct {
    mutex: std.Thread.Mutex = .{},
    failed: bool = false,
    files: std.StringArrayHashMapUnmanaged([:0]const u8) = .{},
    lirs: std.ArrayListUnmanaged(Lir) = .{},
};

pub fn init(allocator: std.mem.Allocator, env: Environment) Compilation {
    return Compilation{
        .allocator = allocator,
        .env = env,
    };
}

pub fn deinit(self: *Compilation) void {
    self.pipeline.files.deinit(self.allocator);
    self.pipeline.lirs.deinit(self.allocator);
}

pub fn push(self: *Compilation, file_path: []const u8) !void {
    const file = try std.fs.cwd().openFile(file_path, .{});
    defer file.close();

    const input = try file.readToEndAllocOptions(self.allocator, std.math.maxInt(u32), null, @alignOf(u8), 0);

    if (input.len == 0) return;

    try self.pipeline.files.put(self.allocator, file_path, input);
}

pub fn start(self: *Compilation) !void {
    var thread_pool: std.Thread.Pool = undefined;
    try thread_pool.init(.{ .allocator = self.allocator });
    defer thread_pool.deinit();

    var wait_group: std.Thread.WaitGroup = .{};

    for (self.pipeline.files.keys(), self.pipeline.files.values()) |file_path, input| {
        thread_pool.spawnWg(&wait_group, compile, .{ self, file_path, input });
    }

    thread_pool.waitAndWork(&wait_group);
}

pub fn compile(self: *Compilation, file_path: []const u8, input: [:0]const u8) void {
    _ = blk: {
        // TODO: Ast is leaking memory..
        const ast = self.parse(file_path, input) orelse break :blk null;

        // TODO: Hir is leaking memory..
        const hir = self.generate(file_path, ast) orelse break :blk null;

        // TODO: Some of the strucures in Lir depend on Hir memory allocated data, we can not free Hir memory here.
        // find a way to free Hir memory when we are done with it or do not depend on it anymore.
        const lir = self.analyze(file_path, hir) orelse break :blk null;

        self.pipeline.mutex.lock();
        defer self.pipeline.mutex.unlock();

        self.pipeline.lirs.append(self.allocator, lir) catch |err| {
            std.debug.print("{s}\n", .{Cli.errorDescription(err)});

            break :blk null;
        };
    } orelse {
        self.pipeline.mutex.lock();
        defer self.pipeline.mutex.unlock();

        self.pipeline.failed = true;
    };
}

pub fn finalize(self: Compilation) std.mem.Allocator.Error!Lir {
    var concatenated_lir: Lir = .{};

    for (self.pipeline.lirs.items) |lir| {
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

pub fn parse(self: Compilation, file_path: []const u8, input: [:0]const u8) ?Ast {
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
            std.debug.print("{s}:{}:{}: {s}\n", .{ file_path, ast_parser.error_info.?.source_loc.line, ast_parser.error_info.?.source_loc.column, ast_parser.error_info.?.message });

            return null;
        },
    };

    return ast;
}

pub fn generate(self: Compilation, file_path: []const u8, ast: Ast) ?Hir {
    var hir_generator = Hir.Generator.init(self.allocator);

    hir_generator.generate(ast) catch |err| switch (err) {
        error.OutOfMemory => {
            std.debug.print("{s}\n", .{Cli.errorDescription(err)});

            return null;
        },

        else => {
            std.debug.print("{s}:{}:{}: {s}\n", .{ file_path, hir_generator.error_info.?.source_loc.line, hir_generator.error_info.?.source_loc.column, hir_generator.error_info.?.message });

            return null;
        },
    };

    return hir_generator.hir;
}

pub fn analyze(self: Compilation, file_path: []const u8, hir: Hir) ?Lir {
    var sema = Sema.init(self.allocator, self.env);
    defer sema.deinit();

    sema.analyze(hir) catch |err| switch (err) {
        error.OutOfMemory => {
            std.debug.print("{s}\n", .{Cli.errorDescription(err)});

            return null;
        },

        else => {
            std.debug.print("{s}:{}:{}: {s}\n", .{ file_path, sema.error_info.?.source_loc.line, sema.error_info.?.source_loc.column, sema.error_info.?.message });

            return null;
        },
    };

    return sema.lir;
}

pub fn render(self: Compilation, lir: Lir) ?[]u8 {
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
