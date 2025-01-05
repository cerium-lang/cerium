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

root_file: File,

env: Environment,

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

pub fn init(allocator: std.mem.Allocator, root_file: File, env: Environment) Compilation {
    return Compilation{
        .allocator = allocator,
        .root_file = root_file,
        .env = env,
    };
}

pub const File = struct {
    path: []const u8,
    buffer: [:0]const u8,
};

/// Parse a file into an sir
pub fn parse(self: Compilation, file: File) ?Sir {
    var sir_parser = Sir.Parser.init(self.allocator, self.env, file.buffer) catch |err| {
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
            std.debug.print("{s}:{}:{}: {s}\n", .{
                file.path,
                sir_parser.error_info.?.source_loc.line,
                sir_parser.error_info.?.source_loc.column,
                sir_parser.error_info.?.message,
            });

            return null;
        },
    };

    return sir_parser.sir;
}

/// Analyze Sir and lower it to Air
pub fn analyze(self: Compilation, file: File, sir: Sir) ?[]Air {
    var sema = Sema.init(self.allocator, &self, file) catch |err| {
        std.debug.print("Error: {s}\n", .{Cli.errorDescription(err)});

        return null;
    };

    defer sema.deinit();

    sema.analyze(sir) catch |err| switch (err) {
        error.OutOfMemory => {
            std.debug.print("Error: {s}\n", .{Cli.errorDescription(err)});

            return null;
        },

        else => {
            std.debug.print("{s}:{}:{}: {s}\n", .{
                file.path,
                sema.error_info.?.source_loc.line,
                sema.error_info.?.source_loc.column,
                sema.error_info.?.message,
            });

            return null;
        },
    };

    const airs = sema.airs.toOwnedSlice(self.allocator) catch |err| {
        std.debug.print("Error: {s}\n", .{Cli.errorDescription(err)});

        return null;
    };

    return airs;
}

/// Emit to an object file or an assembly file
pub fn emit(
    self: Compilation,
    airs: []const Air,
    output_file_path: [:0]const u8,
    output_kind: root.OutputKind,
    code_model: root.CodeModel,
) std.mem.Allocator.Error!void {
    var backend = try LlvmBackend.init(self.allocator, &self);
    defer backend.deinit();

    try backend.render(airs);

    try backend.emit(output_file_path, output_kind, code_model);
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

    var lld_process = std.process.Child.init(&lld_argv, self.allocator);
    lld_process.stdin_behavior = .Inherit;
    lld_process.stdout_behavior = .Inherit;
    lld_process.stderr_behavior = .Inherit;

    const termination = try lld_process.spawnAndWait();

    switch (termination) {
        .Exited => |code| return code,

        else => return 1,
    }
}
