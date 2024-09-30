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

        const input_file_content = blk: {
            const input_file = std.fs.cwd().openFile(options.file_path, .{}) catch |err| {
                std.debug.print("{s}: {s}\n", .{ options.file_path, errorDescription(err) });

                return 1;
            };

            defer input_file.close();

            break :blk input_file.readToEndAllocOptions(self.allocator, std.math.maxInt(u32), null, @alignOf(u8), 0) catch |err| switch (err) {
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

        const cerium_lib_dir = Compilation.Environment.openCeriumLibrary() catch {
            std.debug.print("Error: could not open the cerium library directory\n", .{});

            return 1;
        };

        const env: Compilation.Environment = .{
            .cerium_lib_dir = cerium_lib_dir,
            .source_file_path = options.file_path,
            .target = builtin.target,
        };

        var compilation = Compilation.init(self.allocator, env);

        const ast = compilation.parse(input_file_content) orelse return 1;

        var hir = compilation.generateHir(ast) orelse return 1;

        var lir = compilation.analyzeSemantics(hir) orelse return 1;

        hir.deinit(self.allocator);

        const output = compilation.renderAssembly(lir) orelse return 1;

        defer self.allocator.free(output);

        lir.deinit(self.allocator);

        const input_file_path_stem = std.fs.path.stem(options.file_path);

        var output_file_path = std.ArrayListUnmanaged(u8).initCapacity(self.allocator, input_file_path_stem.len + 2) catch |err| {
            std.debug.print("{s}\n", .{errorDescription(err)});

            return 1;
        };

        defer output_file_path.deinit(self.allocator);

        output_file_path.appendSliceAssumeCapacity(input_file_path_stem);
        output_file_path.appendSliceAssumeCapacity(".s");

        const output_file = std.fs.cwd().createFile(output_file_path.items, .{}) catch |err| {
            std.debug.print("couldn't create output file: {s}\n", .{errorDescription(err)});

            return 1;
        };

        defer output_file.close();

        output_file.writeAll(output) catch |err| {
            std.debug.print("couldn't write the output: {s}\n", .{errorDescription(err)});

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
