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
            input_file_path: []const u8,
            output_file_path: []const u8,
            target: std.Target,
            emit: Emit,

            const Emit = enum {
                assembly,
                object,
                executable,
                none,
            };

            const usage =
                \\Usage:
                \\  {s} compile <file_path>
                \\
                \\Options:
                \\  --output <file_path>    -- specify the output file path
                \\  --target <arch-os-abi>  -- specify the target triple you want to compile to
                \\  --emit <emit-type>      -- specify the type of output you want to emit
                \\                             [assembly, object, executable (default), none (semantic analysis only)]
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
            error.UnexpectedExtraField => "unexpected extra field",
            error.UnknownArchitecture => "unrecognized architecture",
            error.UnknownOperatingSystem, error.MissingOperatingSystem => "unrecognized operating system",
            error.UnknownObjectFormat => "unrecognized object format",
            error.UnknownCpuFeature => "unrecognized cpu feature",
            error.UnknownCpuModel => "unrecognized cpu model",
            error.UnknownApplicationBinaryInterface => "unrecognized application binary interface",
            error.InvalidAbiVersion => "invalid application binary interface version",
            error.InvalidOperatingSystemVersion => "invalid operating system version",

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
                const input_file_path = argument_iterator.next() orelse {
                    std.debug.print(Command.Compile.usage, .{self.program});

                    std.debug.print("Error: expected a file path\n", .{});

                    return null;
                };

                var output_file_path = std.fs.path.stem(input_file_path);

                var target: std.Target = builtin.target;
                var emit: Command.Compile.Emit = .executable;

                if (argument_iterator.next()) |next_argument| {
                    if (std.mem.eql(u8, next_argument, "--output")) {
                        if (argument_iterator.next()) |raw_output_file_path| {
                            output_file_path = raw_output_file_path;
                        } else {
                            std.debug.print(Command.Compile.usage, .{self.program});

                            std.debug.print("Error: expected an output file path\n", .{});

                            return null;
                        }
                    } else if (std.mem.eql(u8, next_argument, "--target")) {
                        if (argument_iterator.next()) |raw_target_triple| {
                            const target_query = std.Target.Query.parse(.{ .arch_os_abi = raw_target_triple }) catch |err| {
                                std.debug.print("Error: could not parse target query: {s}\n", .{errorDescription(err)});

                                return null;
                            };

                            target = std.zig.system.resolveTargetQuery(target_query) catch |err| {
                                std.debug.print("Error: could not resolve target query: {s}\n", .{errorDescription(err)});

                                return null;
                            };
                        } else {
                            std.debug.print(Command.Compile.usage, .{self.program});

                            std.debug.print("Error: expected a target triple\n", .{});

                            return null;
                        }
                    } else if (std.mem.eql(u8, next_argument, "--emit")) {
                        if (argument_iterator.next()) |raw_emit| {
                            if (std.mem.eql(u8, raw_emit, "assembly")) {
                                emit = .assembly;
                            } else if (std.mem.eql(u8, raw_emit, "object")) {
                                emit = .object;
                            } else if (std.mem.eql(u8, raw_emit, "executable")) {
                                emit = .executable;
                            } else if (std.mem.eql(u8, raw_emit, "none")) {
                                emit = .none;
                            } else {
                                std.debug.print(Command.Compile.usage, .{self.program});

                                std.debug.print("Error: unrecognized emit type: {s}\n", .{raw_emit});

                                return null;
                            }
                        } else {
                            std.debug.print(Command.Compile.usage, .{self.program});

                            std.debug.print("Error: expected an emit type\n", .{});

                            return null;
                        }
                    } else {
                        std.debug.print(Command.Compile.usage, .{self.program});

                        std.debug.print("Error: unrecognized argument: {s}\n", .{next_argument});

                        return null;
                    }
                }

                self.command = .{
                    .compile = .{
                        .input_file_path = input_file_path,
                        .output_file_path = output_file_path,
                        .target = target,
                        .emit = emit,
                    },
                };
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

        const runner_file_path = cerium_lib_dir.realpath(
            "std" ++ std.fs.path.sep_str ++ "runners" ++ std.fs.path.sep_str ++ "exe.cerm",
            &runner_file_path_buf,
        ) catch {
            std.debug.print("Error: could not find executable runner file path\n", .{});

            return 1;
        };

        var compilation = Compilation.init(self.allocator, .{
            .cerium_lib_dir = cerium_lib_dir,
            .target = options.target,
        });

        defer compilation.deinit();

        const lir = blk: {
            compilation.put(runner_file_path) catch |err| break :blk err;
            compilation.put(options.input_file_path) catch |err| break :blk err;

            compilation.start() catch |err| break :blk err;

            if (compilation.pipeline.failed) return 1;

            break :blk compilation.finalize();
        } catch |err| {
            std.debug.print("Error: {s}\n", .{errorDescription(err)});

            return 1;
        };

        if (options.emit == .assembly) {
            const assembly_file_path = std.fmt.allocPrintZ(self.allocator, "{s}.s", .{options.output_file_path}) catch |err| {
                std.debug.print("Error: {s}\n", .{errorDescription(err)});

                return 1;
            };

            compilation.emit(lir, assembly_file_path, .assembly) catch |err| {
                std.debug.print("Error: could not emit assembly file: {s}\n", .{errorDescription(err)});

                return 1;
            };
        } else if (options.emit == .object or options.emit == .executable) {
            const object_file_path = std.fmt.allocPrintZ(self.allocator, "{s}{s}", .{
                options.output_file_path,
                options.target.ofmt.fileExt(options.target.cpu.arch),
            }) catch |err| {
                std.debug.print("Error: {s}\n", .{errorDescription(err)});

                return 1;
            };

            compilation.emit(lir, object_file_path, .object) catch |err| {
                std.debug.print("Error: could not emit object file: {s}\n", .{errorDescription(err)});

                return 1;
            };

            if (options.emit == .executable) {
                const linker_exit_code = compilation.link(object_file_path, options.output_file_path) catch |err| {
                    std.debug.print("Error: could not link object file: {s}\n", .{errorDescription(err)});

                    return 1;
                };

                if (linker_exit_code != 0) {
                    std.debug.print(
                        "Error: could not link object file, linker exited with non-zero exit code: {}\n",
                        .{linker_exit_code},
                    );

                    return linker_exit_code;
                }

                std.fs.cwd().deleteFile(object_file_path) catch |err| {
                    std.debug.print("Error: could not delete object file: {s}\n", .{errorDescription(err)});

                    return 1;
                };
            }
        }

        return 0;
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
