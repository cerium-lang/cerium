const std = @import("std");
const builtin = @import("builtin");

const Compilation = @import("compiler/Compilation.zig");

const Driver = @This();

config: Config,

gpa: std.mem.Allocator,

const Config = struct {
    command: ?Command = null,

    const Command = union(enum) {
        compile: Compile,

        const Compile = struct {
            file_path: []const u8,
        };
    };
};

const usage =
    \\Usage: 
    \\  {s} <command> [arguments]
    \\
    \\Commands:
    \\  compile <file_path>     -- compile certain file
    \\
    \\
;

const compile_command_usage =
    \\Usage:
    \\      {s} compile <file_path>
    \\
    \\
;

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

pub fn init(gpa: std.mem.Allocator) Driver {
    return Driver{ .config = .{}, .gpa = gpa };
}

fn parseArgs(self: *Driver, argiterator: *std.process.ArgIterator) bool {
    const program = argiterator.next().?;

    var arg = argiterator.next();

    while (arg != null) : (arg = argiterator.next()) {
        if (std.mem.eql(u8, arg.?, "compile")) {
            const file_path = argiterator.next();

            if (file_path == null) {
                std.debug.print(compile_command_usage, .{program});

                return true;
            }

            self.config.command = .{ .compile = .{ .file_path = file_path.? } };

            return false;
        } else {
            std.debug.print(usage, .{program});

            std.debug.print("Error: {s} is an unknown command\n", .{arg.?});

            return true;
        }
    }

    if (self.config.command == null) {
        std.debug.print(usage, .{program});

        std.debug.print("Error: no command provided\n", .{});

        return true;
    }

    return false;
}

pub fn run(self: *Driver, argiterator: *std.process.ArgIterator) u8 {
    if (self.parseArgs(argiterator)) return 1;

    switch (self.config.command.?) {
        .compile => return self.runCompileCommand(),
    }

    return 0;
}

fn runCompileCommand(self: *Driver) u8 {
    const options = self.config.command.?.compile;

    const input_file = std.fs.cwd().openFile(options.file_path, .{}) catch |err| {
        std.debug.print("{s}: {s}\n", .{ options.file_path, errorDescription(err) });

        return 1;
    };
    defer input_file.close();

    const input_file_content = input_file.reader().readAllAlloc(self.gpa, std.math.maxInt(u32)) catch |err| {
        std.debug.print("{s}: {s}\n", .{ options.file_path, errorDescription(err) });

        return 1;
    };
    defer self.gpa.free(input_file_content);

    var input_file_content_z = @as([:0]u8, @ptrCast(input_file_content));
    input_file_content_z[input_file_content_z.len] = 0;

    var compilation = Compilation.init(self.gpa, builtin.target, options.file_path);

    const output_assembly = compilation.compile(input_file_content_z) orelse {
        return 1;
    };

    const output_file = std.fs.cwd().createFile("a.out.s", .{}) catch |err| {
        std.debug.print("couldn't create output file: {s}\n", .{errorDescription(err)});

        return 1;
    };
    defer output_file.close();

    output_file.writer().writeAll(output_assembly) catch |err| {
        std.debug.print("couldn't write the output assembly: {s}", .{errorDescription(err)});

        return 1;
    };

    return 0;
}
