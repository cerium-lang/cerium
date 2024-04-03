const std = @import("std");

const Parser = @import("ast.zig").Parser;
const CodeGen = @import("CodeGen.zig");

const Compilation = @This();

env: Environment,

gpa: std.mem.Allocator,

const Environment = struct { source_file_path: []const u8, target: std.Target };

pub fn init(gpa: std.mem.Allocator, target: std.Target, source_file_path: []const u8) Compilation {
    return Compilation{ .gpa = gpa, .env = .{ .target = target, .source_file_path = source_file_path } };
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

pub fn compile(self: *Compilation, input: [:0]const u8) ?[]const u8 {
    var parser = Parser.init(self.gpa, input) catch |err| {
        std.debug.print("{s}\n", .{errorDescription(err)});

        return null;
    };

    const root = parser.parseRoot() catch |err| switch (err) {
        error.OutOfMemory => {
            std.debug.print("{s}\n", .{errorDescription(err)});

            return null;
        },

        else => {
            std.debug.print("{s}:{}:{}: {s}\n", .{ self.env.source_file_path, parser.error_info.?.loc.line, parser.error_info.?.loc.column, parser.error_info.?.message });

            return null;
        },
    };

    var codegen = CodeGen.init(self.gpa);

    const ir = codegen.gen(root) catch |err| switch (err) {
        error.OutOfMemory => {
            std.debug.print("{s}\n", .{errorDescription(err)});

            return null;
        },

        else => {
            std.debug.print("{s}:{}:{}: {s}\n", .{ self.env.source_file_path, codegen.error_info.?.loc.line, codegen.error_info.?.loc.column, codegen.error_info.?.message });

            return null;
        },
    };

    const output_assembly = ir.render(self.gpa, self.env.target) catch |err| switch (err) {
        error.UnsupportedTarget => {
            std.debug.print("{s} is not spported yet\n", .{self.env.target.cpu.arch.genericName()});

            return null;
        },

        else => {
            std.debug.print("{s}\n", .{errorDescription(err)});

            return null;
        },
    };

    return output_assembly;
}
