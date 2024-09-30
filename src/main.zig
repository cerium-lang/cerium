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
            error.SystemResources => "ran out of system resources",
            error.FatalError => "a fatal error occurred",
            error.Unexpected => "an unexpected error occurred",
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
            std.debug.print("Error: could not find the standard library executable runner file path\n", .{});

            return 1;
        };

        const runner_file_content = blk: {
            const file = cerium_lib_dir.openFile("std/runners/exe.cerm", .{}) catch |err| {
                std.debug.print("{s}: {s}\n", .{ options.file_path, errorDescription(err) });

                return 1;
            };

            defer file.close();

            break :blk file.readToEndAllocOptions(self.allocator, std.math.maxInt(u32), null, @alignOf(u8), 0) catch |err| switch (err) {
                error.OutOfMemory => {
                    std.debug.print("{s}\n", .{errorDescription(err)});

                    return 1;
                },

                else => {
                    std.debug.print("{s}: {s}\n", .{ options.file_path, errorDescription(err) });

                    return 1;
                },
            };
        };

        if (runner_file_content.len == 0) {
            return 0;
        }

        defer self.allocator.free(runner_file_content);

        const input_file_content = blk: {
            const file = std.fs.cwd().openFile(options.file_path, .{}) catch |err| {
                std.debug.print("{s}: {s}\n", .{ options.file_path, errorDescription(err) });

                return 1;
            };

            defer file.close();

            break :blk file.readToEndAllocOptions(self.allocator, std.math.maxInt(u32), null, @alignOf(u8), 0) catch |err| switch (err) {
                error.OutOfMemory => {
                    std.debug.print("{s}\n", .{errorDescription(err)});

                    return 1;
                },

                else => {
                    std.debug.print("{s}: {s}\n", .{ options.file_path, errorDescription(err) });

                    return 1;
                },
            };
        };

        if (input_file_content.len == 0) {
            return 0;
        }

        defer self.allocator.free(input_file_content);

        const env: Compilation.Environment = .{
            .cerium_lib_dir = cerium_lib_dir,
            .source_file_path = options.file_path,
            .target = builtin.target,
        };

        var compilation = Compilation.init(self.allocator, env);

        const input_lir = compilation.compileLir(input_file_content) orelse return 1;

        compilation.env.source_file_path = runner_file_path;
        const runner_lir = compilation.compileLir(runner_file_content) orelse return 1;

        const lir = compilation.concatLir(&.{ runner_lir, input_lir }) catch |err| {
            std.debug.print("{s}\n", .{errorDescription(err)});

            return 1;
        };

        const output = compilation.renderAssembly(lir) orelse return 1;

        defer self.allocator.free(output);

        const input_file_path_stem = std.fs.path.stem(options.file_path);

        var output_file_path = std.ArrayListUnmanaged(u8).initCapacity(self.allocator, input_file_path_stem.len + 2) catch |err| {
            std.debug.print("{s}\n", .{errorDescription(err)});

            return 1;
        };

        defer output_file_path.deinit(self.allocator);

        output_file_path.appendSliceAssumeCapacity(input_file_path_stem);
        output_file_path.appendSliceAssumeCapacity(".s");

        const output_file = std.fs.cwd().createFile(output_file_path.items, .{}) catch |err| {
            std.debug.print("could not create output file: {s}\n", .{errorDescription(err)});

            return 1;
        };

        defer output_file.close();

        output_file.writeAll(output) catch |err| {
            std.debug.print("could not write the output: {s}\n", .{errorDescription(err)});

            return 1;
        };

        return 0;
    }

    fn executeHelpCommand(self: Cli) u8 {
        std.debug.print(usage, .{self.program});

        return 0;
    }
};

pub fn main() u8 {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};

    var arena_instance = std.heap.ArenaAllocator.init(gpa.allocator());
    defer arena_instance.deinit();

    const allocator = arena_instance.allocator();

    var argument_iterator = std.process.ArgIterator.initWithAllocator(allocator) catch {
        std.debug.print("ran out of memory\n", .{});

        return 1;
    };

    defer argument_iterator.deinit();

    const cli = Cli.parse(allocator, &argument_iterator) orelse return 1;

    switch (cli.command.?) {
        .compile => return cli.executeCompileCommand(),
        .help => return cli.executeHelpCommand(),
    }
}
