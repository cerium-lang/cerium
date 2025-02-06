//! Compilation.
//!
//! A wrapper around the compilation pipeline of Barq.

const std = @import("std");
const root = @import("root");

const Air = @import("Air.zig");
const Symbol = @import("Symbol.zig");
const Sir = @import("Sir.zig");
const Sema = @import("Sema.zig");

const LlvmBackend = @import("backend/LlvmBackend.zig");

const Compilation = @This();

allocator: std.mem.Allocator,

root_file: File,

compiled_files: std.StringHashMapUnmanaged(CompiledFile) = .{},

env: Environment,

pub const Environment = struct {
    barq_lib_dir: std.fs.Dir,
    target: std.Target,

    /// Open the Barq library directory by finding `lib/barq` or `lib`
    pub fn openBarqLibrary() !std.fs.Dir {
        var self_exe_dir_path_buf: [std.fs.max_path_bytes]u8 = undefined;

        const self_exe_dir_path = try std.fs.selfExeDirPath(&self_exe_dir_path_buf);

        const self_exe_dir = try std.fs.openDirAbsolute(self_exe_dir_path, .{});

        // We start from the executable directory, and iterate upwards
        var dir = self_exe_dir;

        var opened = false;

        while (!opened) {
            opened = true;

            // We first try to open `lib/barq` directory so we differentiate between
            // `/usr/lib` and `/usr/lib/barq` if the executable is in `/usr/bin`
            dir = dir.openDir("lib" ++ std.fs.path.sep_str ++ "barq", .{}) catch |err| switch (err) {
                error.FileNotFound => blk: {
                    // Ok so we didn't find `lib/barq` let's now try the more generic `lib`
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

pub const CompiledFile = struct {
    path: []const u8,
    buffer: [:0]const u8,
    sir: Sir,
    scope: Symbol.Scope(Sema.Variable),
};

pub fn emit(
    self: Compilation,
    air: Air,
    output_file_path: [:0]const u8,
    output_kind: root.OutputKind,
    code_model: root.CodeModel,
) std.mem.Allocator.Error!void {
    var backend = try LlvmBackend.init(self.allocator, &self);
    defer backend.deinit();

    try backend.render(air);

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
