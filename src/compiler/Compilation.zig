const std = @import("std");
const root = @import("root");

const Ast = @import("Ast.zig");
const Hir = @import("Hir.zig");
const Lir = @import("Lir.zig");
const Sema = @import("Sema.zig");
const Cli = root.Cli;

const LlvmBackend = @import("backend/LlvmBackend.zig");

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

            dir = dir.openDir("lib" ++ std.fs.path.sep_str ++ "cerium", .{}) catch |err| switch (err) {
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

pub fn put(self: *Compilation, file_path: []const u8) !void {
    if (self.pipeline.files.get(file_path) != null) return;

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
            std.debug.print("Error: {s}\n", .{Cli.errorDescription(err)});

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
        try concatenated_lir.instructions.appendSlice(self.allocator, lir.instructions.items);
    }

    return concatenated_lir;
}

pub fn parse(self: Compilation, file_path: []const u8, input: [:0]const u8) ?Ast {
    var ast_parser = Ast.Parser.init(self.allocator, input) catch |err| {
        std.debug.print("Error: {s}\n", .{Cli.errorDescription(err)});

        return null;
    };

    defer ast_parser.deinit();

    const ast = ast_parser.parse() catch |err| switch (err) {
        error.OutOfMemory => {
            std.debug.print("Error: {s}\n", .{Cli.errorDescription(err)});

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
    var hir_generator = Hir.Generator.init(self.allocator, self.env);

    hir_generator.generate(ast) catch |err| switch (err) {
        error.OutOfMemory => {
            std.debug.print("Error: {s}\n", .{Cli.errorDescription(err)});

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
    var sema = Sema.init(self.allocator, self.env, hir);
    defer sema.deinit();

    sema.analyze() catch |err| switch (err) {
        error.OutOfMemory => {
            std.debug.print("Error: {s}\n", .{Cli.errorDescription(err)});

            return null;
        },

        else => {
            std.debug.print("{s}:{}:{}: {s}\n", .{ file_path, sema.error_info.?.source_loc.line, sema.error_info.?.source_loc.column, sema.error_info.?.message });

            return null;
        },
    };

    return sema.lir;
}

pub const OutputKind = enum {
    object,
    assembly,
};

pub fn emit(self: Compilation, lir: Lir, output_file_path: [:0]const u8, output_kind: OutputKind) std.mem.Allocator.Error!void {
    var backend = LlvmBackend.init(self.allocator, self.env.target, lir);
    defer backend.deinit();

    try backend.render();

    try backend.emit(output_file_path, output_kind);
}

pub fn link(self: Compilation, object_file_path: []const u8, output_file_path: []const u8) !u8 {
    const lld = switch (self.env.target.ofmt) {
        .coff => "lld-link",
        .elf => "ld.lld",
        .macho => "ld64.lld",
        .wasm => "wasm-ld",

        else => return error.UnsupportedObjectFormat,
    };

    const lld_argv: [4][]const u8 = .{
        lld,
        object_file_path,
        "-o",
        output_file_path,
    };

    if (!std.process.can_spawn) {
        @compileError("TODO: use lld library if spawning is not supported");
    }

    const lld_process = try std.process.Child.run(.{
        .allocator = self.allocator,
        .argv = &lld_argv,
    });

    std.debug.print("{s}", .{lld_process.stderr});

    switch (lld_process.term) {
        .Exited => |code| return code,

        else => return 1,
    }
}
