const std = @import("std");
const builtin = @import("builtin");

const Compilation = @import("compiler/Compilation.zig");

pub const Cli = struct {
    allocator: std.mem.Allocator,

    program: []const u8,
    command: ?Command = null,

    const Command = union(enum) {
        compile: Compile,
        help,

        const Compile = struct {
            file_path: []const u8,

            const usage =
                \\Usage:
                \\  {s} compile <file_path>
                \\
                \\
            ;
        };
    };

    const usage =
        \\Usage:
        \\  {s} <command> [arguments]
        \\
        \\Commands:
        \\  compile <file_path>     -- compile certain file
        \\  help                    -- print this help message
        \\
        \\
    ;

    pub fn errorDescription(e: anyerror) []const u8 {
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
            error.ThreadQuotaExceeded => "ran out of threads",
            error.LockedMemoryLimitExceeded => "ran out of locked memory",
            error.SystemResources => "ran out of system resources",
            error.FatalError => "a fatal error occurred",
            error.Unexpected => "an unexpected error occurred",
            error.UnsupportedObjectFormat => "unsupported object format",
            else => @errorName(e),
        };
    }

    fn parse(allocator: std.mem.Allocator, argument_iterator: *std.process.ArgIterator) ?Cli {
        var self: Cli = .{
            .allocator = allocator,
            .program = argument_iterator.next().?,
        };

        while (argument_iterator.next()) |argument| {
            if (std.mem.eql(u8, argument, "compile")) {
                const maybe_file_path = argument_iterator.next();

                if (maybe_file_path == null) {
                    std.debug.print(Command.Compile.usage, .{self.program});

                    std.debug.print("Error: expected a file path\n", .{});

                    return null;
                }

                const file_path = maybe_file_path.?;

                self.command = .{ .compile = .{ .file_path = file_path } };
            } else if (std.mem.eql(u8, argument, "help")) {
                self.command = .help;
            } else {
                std.debug.print(usage, .{self.program});

                std.debug.print("Error: {s} is an unknown command\n", .{argument});

                return null;
            }
        }

        if (self.command == null) {
            std.debug.print(usage, .{self.program});

            std.debug.print("Error: no command provided\n", .{});

            return null;
        }

        return self;
    }

    fn executeCompileCommand(self: Cli) u8 {
        const options = self.command.?.compile;

        const cerium_lib_dir = Compilation.Environment.openCeriumLibrary() catch {
            std.debug.print("Error: could not open the cerium library directory\n", .{});

            return 1;
        };

        var runner_file_path_buf: [std.fs.max_path_bytes]u8 = undefined;

        const runner_file_path = cerium_lib_dir.realpath("std/runners/exe.cerm", &runner_file_path_buf) catch {
            std.debug.print("Error: could not find the executable runner file path\n", .{});

            return 1;
        };

        const target = builtin.target;

        var compilation = Compilation.init(self.allocator, .{ .cerium_lib_dir = cerium_lib_dir, .target = target });
        defer compilation.deinit();

        const lir = blk: {
            compilation.put(runner_file_path) catch |err| break :blk err;
            compilation.put(options.file_path) catch |err| break :blk err;

            compilation.start() catch |err| break :blk err;

            if (compilation.pipeline.failed) return 1;

            break :blk compilation.finalize();
        } catch |err| {
            std.debug.print("Error: {s}\n", .{errorDescription(err)});

            return 1;
        };

        const output_file_path = std.fs.path.stem(options.file_path);

        const object_file_path = std.fmt.allocPrintZ(self.allocator, "{s}{s}", .{ output_file_path, target.ofmt.fileExt(target.cpu.arch) }) catch |err| {
            std.debug.print("Error: {s}\n", .{errorDescription(err)});

            return 1;
        };

        compilation.emit(lir, object_file_path, .object) catch |err| {
            std.debug.print("Error: {s}\n", .{errorDescription(err)});

            return 1;
        };

        return compilation.link(object_file_path, output_file_path) catch |err| {
            std.debug.print("Error: {s}\n", .{errorDescription(err)});

            return 1;
        };
    }

    fn executeHelpCommand(self: Cli) u8 {
        std.debug.print(usage, .{self.program});

        return 0;
    }
};

pub fn main() u8 {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};

    const allocator = gpa.allocator();

    var argument_iterator = std.process.ArgIterator.initWithAllocator(allocator) catch |err| {
        std.debug.print("Error: {s}\n", .{Cli.errorDescription(err)});

        return 1;
    };

    defer argument_iterator.deinit();

    const cli = Cli.parse(allocator, &argument_iterator) orelse return 1;

    switch (cli.command.?) {
        .compile => return cli.executeCompileCommand(),
        .help => return cli.executeHelpCommand(),
    }
}
