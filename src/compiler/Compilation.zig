const std = @import("std");

const Ast = @import("Ast.zig");

const Ir = @import("Ir.zig");

const Compilation = @This();

allocator: std.mem.Allocator,

env: Environment,

pub const Environment = struct {
    source_file_path: []const u8,
    target: std.Target,
};

pub fn init(allocator: std.mem.Allocator, env: Environment) Compilation {
    return Compilation{
        .allocator = allocator,
        .env = env,
    };
}

fn errorDescription(e: anyerror) []const u8 {
    return switch (e) {
        error.OutOfMemory => "ran out of memory",
        error.FileNotFound => "no such file or directory",
        error.IsDir => "is a directory",
        error.NotDir => "is not a directory",
        error.NotOpenForReading => "is not open for reading",
        error.NotOpenForWriting => "is not open for writing",
        error.InvalidUtf8 => "invalid UTF-8",
        error.FileBusy => "file is busy",
        error.NameTooLong => "name is too long",
        error.AccessDenied => "access denied",
        error.FileTooBig, error.StreamTooLong => "file is too big",
        error.ProcessFdQuotaExceeded, error.SystemFdQuotaExceeded => "ran out of file descriptors",
        error.SystemResources => "ran out of system resources",
        error.FatalError => "a fatal error occurred",
        error.Unexpected => "an unexpected error occurred",
        else => @errorName(e),
    };
}

pub fn parse(self: *Compilation, input: [:0]const u8) ?Ast {
    var ast_parser = Ast.Parser.init(self.allocator, input) catch |err| {
        std.debug.print("{s}\n", .{errorDescription(err)});

        return null;
    };

    const ast = ast_parser.parse() catch |err| switch (err) {
        error.OutOfMemory => {
            std.debug.print("{s}\n", .{errorDescription(err)});

            return null;
        },

        else => {
            std.debug.print("{s}:{}:{}: {s}\n", .{ self.env.source_file_path, ast_parser.error_info.?.source_loc.line, ast_parser.error_info.?.source_loc.column, ast_parser.error_info.?.message });

            return null;
        },
    };

    return ast;
}

pub fn generateIr(self: *Compilation, ast: Ast) ?Ir {
    var ir_generator = Ir.Generator.init(self.allocator);

    const ir = ir_generator.generate(ast) catch |err| switch (err) {
        error.OutOfMemory => {
            std.debug.print("{s}\n", .{errorDescription(err)});

            return null;
        },

        else => {
            std.debug.print("{s}:{}:{}: {s}\n", .{ self.env.source_file_path, ir_generator.error_info.?.source_loc.line, ir_generator.error_info.?.source_loc.column, ir_generator.error_info.?.message });

            return null;
        },
    };

    return ir;
}

pub fn renderAssembly(self: *Compilation, ir: Ir) ?[]const u8 {
    const Aarch64Backend = @import("backends/assembly/Aarch64Backend.zig");
    const x86_64Backend = @import("backends/assembly/x86_64Backend.zig");

    return switch (self.env.target.cpu.arch) {
        .aarch64 => blk: {
            var backend = Aarch64Backend.init(self.allocator, ir);

            backend.render() catch |err| {
                std.debug.print("{s}\n", .{errorDescription(err)});

                return null;
            };

            break :blk backend.dump() catch null;
        },

        .x86_64 => blk: {
            var backend = x86_64Backend.init(self.allocator, ir);

            backend.render() catch |err| {
                std.debug.print("{s}\n", .{errorDescription(err)});

                return null;
            };

            break :blk backend.dump() catch null;
        },

        else => {
            std.debug.print("{s} is not supported yet", .{self.env.target.cpu.arch.genericName()});

            return null;
        },
    };
}
