const std = @import("std");
const builtin = @import("builtin");

const Compilation = @import("compiler/Compilation.zig");

const Driver = @This();

gpa: std.mem.Allocator,

cli: CLI,

const CLI = struct {
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
    \\  {s} compile <file_path>
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
    return Driver{
        .gpa = gpa,
        .cli = .{},
    };
}

fn parseArgs(self: *Driver, arg_iterator: *std.process.ArgIterator) bool {
    const program = arg_iterator.next().?;

    while (arg_iterator.next()) |arg| {
        if (std.mem.eql(u8, arg, "compile")) {
            if (arg_iterator.next()) |file_path| {
                self.cli.command = .{ .compile = .{ .file_path = file_path } };

                return false;
            } else {
                std.debug.print(compile_command_usage, .{program});

                std.debug.print("Error: expected at least 1 file path\n", .{});

                return true;
            }
        } else {
            std.debug.print(usage, .{program});

            std.debug.print("Error: {s} is an unknown command\n", .{arg});

            return true;
        }
    }

    if (self.cli.command == null) {
        std.debug.print(usage, .{program});

        std.debug.print("Error: no command provided\n", .{});

        return true;
    }

    return false;
}

pub fn run(self: *Driver, arg_iterator: *std.process.ArgIterator) u8 {
    if (self.parseArgs(arg_iterator)) return 1;

    switch (self.cli.command.?) {
        .compile => return self.runCompileCommand(),
    }

    return 0;
}

fn readAllFileSentinel(gpa: std.mem.Allocator, file_path: []const u8) ?[:0]const u8 {
    const file = std.fs.cwd().openFile(file_path, .{}) catch |err| {
        std.debug.print("{s}: {s}\n", .{ file_path, errorDescription(err) });

        return null;
    };

    defer file.close();

    const file_content = file.reader().readAllAlloc(gpa, std.math.maxInt(u32)) catch |err| {
        std.debug.print("{s}: {s}\n", .{ file_path, errorDescription(err) });

        return null;
    };

    var file_content_z = @as([:0]u8, @ptrCast(file_content));
    file_content_z[file_content_z.len] = 0;

    return file_content_z;
}

fn runCompileCommand(self: *Driver) u8 {
    const options = self.cli.command.?.compile;

    const input_file_content = readAllFileSentinel(self.gpa, options.file_path) orelse return 1;
    defer self.gpa.free(input_file_content);

    var compilation = Compilation.init(self.gpa, .{ .source_file_path = options.file_path, .target = builtin.target });

    const root = compilation.parse(input_file_content) orelse return 1;

    const ir = compilation.compile_ir(root) orelse return 1;

    const input_file_path_stem = std.fs.path.stem(options.file_path);

    var output_file_path = std.ArrayList(u8).initCapacity(self.gpa, input_file_path_stem.len + 2) catch |err| {
        std.debug.print("{s}\n", .{errorDescription(err)});

        return 1;
    };

    output_file_path.appendSliceAssumeCapacity(input_file_path_stem);
    output_file_path.appendSliceAssumeCapacity(".s");

    const owned_output_file_path = output_file_path.toOwnedSlice() catch |err| {
        std.debug.print("{s}\n", .{errorDescription(err)});

        return 1;
    };

    defer self.gpa.free(owned_output_file_path);

    const output_file = std.fs.cwd().createFile(owned_output_file_path, .{}) catch |err| {
        std.debug.print("couldn't create output file: {s}\n", .{errorDescription(err)});

        return 1;
    };

    defer output_file.close();

    const output_assembly = compilation.render_ir(ir) orelse return 1;

    output_file.writer().writeAll(output_assembly) catch |err| {
        std.debug.print("couldn't write the output assembly: {s}\n", .{errorDescription(err)});

        return 1;
    };

    return 0;
}
