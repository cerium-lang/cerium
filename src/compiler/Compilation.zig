//! Compilation.
//!
//! A wrapper around the compilation pipeline of Cerium.

const std = @import("std");
const root = @import("root");

const Sir = @import("Sir.zig");
const Air = @import("Air.zig");
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

    /// Open the Cerium library directory by finding `lib/cerium` or `lib`
    pub fn openCeriumLibrary() !std.fs.Dir {
        var self_exe_dir_path_buf: [std.fs.max_path_bytes]u8 = undefined;

        const self_exe_dir_path = try std.fs.selfExeDirPath(&self_exe_dir_path_buf);

        const self_exe_dir = try std.fs.openDirAbsolute(self_exe_dir_path, .{});

        // We start from the executable directory, and iterate upwards
        var dir = self_exe_dir;

        var opened = false;

        while (!opened) {
            opened = true;

            // We first try to open `lib/cerium` directory so we differentiate between
            // `/usr/lib` and `/usr/lib/cerium` if the executable is in `/usr/bin`
            dir = dir.openDir("lib" ++ std.fs.path.sep_str ++ "cerium", .{}) catch |err| switch (err) {
                error.FileNotFound => blk: {
                    // Ok so we didn't find `lib/cerium` let's now try the more generic `lib`
                    break :blk dir.openDir("lib", .{}) catch |another_err| switch (another_err) {
                        error.FileNotFound => {
                            opened = false;

                            // We still didn't find any of those, so we need to go up one directory
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
    airs: std.ArrayListUnmanaged(Air) = .{},
};

pub fn init(allocator: std.mem.Allocator, env: Environment) Compilation {
    return Compilation{
        .allocator = allocator,
        .env = env,
    };
}

pub fn deinit(self: *Compilation) void {
    self.pipeline.files.deinit(self.allocator);
    self.pipeline.airs.deinit(self.allocator);
}

/// Add a file to the compilation pipeline and associate it with its path
pub fn put(self: *Compilation, file_path: []const u8) !void {
    if (self.pipeline.files.get(file_path) != null) return;

    const file = try std.fs.cwd().openFile(file_path, .{});
    defer file.close();

    const input = try file.readToEndAllocOptions(self.allocator, std.math.maxInt(u32), null, @alignOf(u8), 0);
    if (input.len == 0) return;

    try self.pipeline.files.put(self.allocator, file_path, input);
}

/// Start the compilation pipeline and make it run in parallel
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

/// Compile a file and append the result to the compilation pipeline's airs
pub fn compile(self: *Compilation, file_path: []const u8, input: [:0]const u8) void {
    _ = blk: {
        // TODO: Sir is leaking memory..
        const sir = self.parse(file_path, input) orelse break :blk null;

        // TODO: Some of the strucures in Air depend on Sir memory allocated data, we can not free Sir memory here.
        // find a way to free Sir memory when we are done with it or do not depend on it anymore.
        const air = self.analyze(file_path, sir) orelse break :blk null;

        self.pipeline.mutex.lock();
        defer self.pipeline.mutex.unlock();

        self.pipeline.airs.append(self.allocator, air) catch |err| {
            std.debug.print("Error: {s}\n", .{Cli.errorDescription(err)});

            break :blk null;
        };
    } orelse {
        self.pipeline.mutex.lock();
        defer self.pipeline.mutex.unlock();

        self.pipeline.failed = true;
    };
}

/// Concatenate all the airs in the compilation pipeline and return the result
pub fn finalize(self: Compilation) std.mem.Allocator.Error!Air {
    var concatenated_air: Air = .{};

    for (self.pipeline.airs.items) |air| {
        try concatenated_air.instructions.appendSlice(self.allocator, air.instructions.items);
    }

    return concatenated_air;
}

/// Parse a file into an sir
pub fn parse(self: Compilation, file_path: []const u8, input: [:0]const u8) ?Sir {
    var sir_parser = Sir.Parser.init(self.allocator, self.env, input) catch |err| {
        std.debug.print("Error: {s}\n", .{Cli.errorDescription(err)});

        return null;
    };

    defer sir_parser.deinit();

    sir_parser.parse() catch |err| switch (err) {
        error.OutOfMemory => {
            std.debug.print("Error: {s}\n", .{Cli.errorDescription(err)});

            return null;
        },

        else => {
            std.debug.print("{s}:{}:{}: {s}\n", .{ file_path, sir_parser.error_info.?.source_loc.line, sir_parser.error_info.?.source_loc.column, sir_parser.error_info.?.message });

            return null;
        },
    };

    return sir_parser.sir;
}

/// Analyze Sir and lower it to Air
pub fn analyze(self: Compilation, file_path: []const u8, sir: Sir) ?Air {
    var sema = Sema.init(self.allocator, self.env, sir) catch |err| {
        std.debug.print("Error: {s}\n", .{Cli.errorDescription(err)});

        return null;
    };

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

    return sema.air;
}

pub const OutputKind = enum {
    object,
    assembly,
};

/// Emit Air to an object file or an assembly file
pub fn emit(self: Compilation, air: Air, output_file_path: [:0]const u8, output_kind: OutputKind) std.mem.Allocator.Error!void {
    var backend = LlvmBackend.init(self.allocator, self.env.target, air);
    defer backend.deinit();

    try backend.render();

    try backend.emit(output_file_path, output_kind);
}

/// Link an object file into an executable file
pub fn link(self: Compilation, object_file_path: []const u8, output_file_path: []const u8) !u8 {
    const lld = switch (self.env.target.ofmt) {
        .coff => "lld-link",
        .elf => "ld.lld",
        .macho => "ld64.lld",
        .wasm => "wasm-ld",

        else => return error.UnknownObjectFormat,
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
