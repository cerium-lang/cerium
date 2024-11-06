const std = @import("std");
const builtin = @import("builtin");

const Compilation = @import("compiler/Compilation.zig");

pub const OutputKind = enum {
    assembly,
    object,
    executable,
    none,
};

pub const RunnerKind = enum {
    executable,
    none,
};

pub const Cli = struct {
    allocator: std.mem.Allocator,

    program: []const u8,
    command: ?Command = null,

    const Command = union(enum) {
        compile: Compile,
        help,

        const Compile = struct {
            input_file_path: []const u8,
            maybe_output_file_path: ?[]const u8,
            output_kind: OutputKind,
            runner_kind: RunnerKind,
            target: std.Target,

            const usage =
                \\Usage:
                \\  {s} compile <input-file-path>
                \\
                \\Options:
                \\  --output <output-file-path>    -- specify the output file path
                \\  --emit <output-kind>           -- specify the output kind
                \\                                    [assembly, object, executable (default), none]
                \\  --runner <runner-kind>         -- specify the runner kind
                \\                                    [executable (default), none]
                \\  --target <arch-os-abi>         -- specify the target query
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
        \\  compile <input-file-path>     -- compile certain file
        \\  help                          -- print this help message
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

                    std.debug.print("Error: expected input file path\n", .{});

                    return null;
                };

                var maybe_output_file_path: ?[]const u8 = null;
                var output_kind: OutputKind = .executable;
                var runner_kind: RunnerKind = .executable;
                var target: std.Target = builtin.target;

                while (argument_iterator.next()) |next_argument| {
                    if (std.mem.eql(u8, next_argument, "--output")) {
                        maybe_output_file_path = argument_iterator.next() orelse {
                            std.debug.print(Command.Compile.usage, .{self.program});

                            std.debug.print("Error: expected output file path\n", .{});

                            return null;
                        };
                    } else if (std.mem.eql(u8, next_argument, "--emit")) {
                        if (argument_iterator.next()) |raw_output_kind| {
                            if (std.mem.eql(u8, raw_output_kind, "assembly")) {
                                output_kind = .assembly;
                            } else if (std.mem.eql(u8, raw_output_kind, "object")) {
                                output_kind = .object;
                            } else if (std.mem.eql(u8, raw_output_kind, "executable")) {
                                output_kind = .executable;
                            } else if (std.mem.eql(u8, raw_output_kind, "none")) {
                                output_kind = .none;
                            } else {
                                std.debug.print(Command.Compile.usage, .{self.program});

                                std.debug.print("Error: unrecognized output kind: {s}\n", .{raw_output_kind});

                                return null;
                            }
                        } else {
                            std.debug.print(Command.Compile.usage, .{self.program});

                            std.debug.print("Error: expected output kind\n", .{});

                            return null;
                        }
                    } else if (std.mem.eql(u8, next_argument, "--runner")) {
                        if (argument_iterator.next()) |raw_runner_kind| {
                            if (std.mem.eql(u8, raw_runner_kind, "executable")) {
                                runner_kind = .executable;
                            } else if (std.mem.eql(u8, raw_runner_kind, "none")) {
                                runner_kind = .none;
                            } else {
                                std.debug.print(Command.Compile.usage, .{self.program});

                                std.debug.print("Error: unrecognized runner kind: {s}\n", .{raw_runner_kind});

                                return null;
                            }
                        } else {
                            std.debug.print(Command.Compile.usage, .{self.program});

                            std.debug.print("Error: expected runner kind\n", .{});

                            return null;
                        }
                    } else if (std.mem.eql(u8, next_argument, "--target")) {
                        if (argument_iterator.next()) |raw_target_query| {
                            const target_query = std.Target.Query.parse(.{ .arch_os_abi = raw_target_query }) catch |err| {
                                std.debug.print("Error: could not parse target query: {s}\n", .{errorDescription(err)});

                                return null;
                            };

                            target = std.zig.system.resolveTargetQuery(target_query) catch |err| {
                                std.debug.print("Error: could not resolve target query: {s}\n", .{errorDescription(err)});

                                return null;
                            };
                        } else {
                            std.debug.print(Command.Compile.usage, .{self.program});

                            std.debug.print("Error: expected target query\n", .{});

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
                        .maybe_output_file_path = maybe_output_file_path,
                        .output_kind = output_kind,
                        .runner_kind = runner_kind,
                        .target = target,
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

        var compilation = Compilation.init(self.allocator, .{
            .cerium_lib_dir = cerium_lib_dir,
            .target = options.target,
        });

        defer compilation.deinit();

        if (options.runner_kind == .executable) {
            var runner_file_path_buf: [std.fs.max_path_bytes]u8 = undefined;

            const runner_file_path = cerium_lib_dir.realpath(
                "std" ++ std.fs.path.sep_str ++ "runners" ++ std.fs.path.sep_str ++ "exe.cerm",
                &runner_file_path_buf,
            ) catch {
                std.debug.print("Error: could not find executable runner file path\n", .{});

                return 1;
            };

            compilation.put(runner_file_path) catch |err| {
                std.debug.print("Error: {s}\n", .{errorDescription(err)});

                return 1;
            };
        }

        compilation.put(options.input_file_path) catch |err| {
            std.debug.print("Error: {s}\n", .{errorDescription(err)});

            return 1;
        };

        compilation.compileAll() catch |err| {
            std.debug.print("Error: {s}\n", .{errorDescription(err)});

            return 1;
        };

        if (compilation.pipeline.failed) return 1;

        if (options.output_kind == .assembly) {
            const assembly_file_path = std.fmt.allocPrintZ(self.allocator, "{s}{s}", .{
                options.maybe_output_file_path orelse std.fs.path.stem(options.input_file_path),
                if (options.maybe_output_file_path != null) "" else ".s",
            }) catch |err| {
                std.debug.print("Error: {s}\n", .{errorDescription(err)});

                return 1;
            };

            defer self.allocator.free(assembly_file_path);

            compilation.emit(assembly_file_path, options.output_kind) catch |err| {
                std.debug.print("Error: could not emit assembly file: {s}\n", .{errorDescription(err)});

                return 1;
            };
        } else if (options.output_kind == .object or options.output_kind == .executable) {
            const object_file_path = std.fmt.allocPrintZ(self.allocator, "{s}{s}", .{
                options.maybe_output_file_path orelse std.fs.path.stem(options.input_file_path),
                if (options.maybe_output_file_path != null) "" else options.target.ofmt.fileExt(options.target.cpu.arch),
            }) catch |err| {
                std.debug.print("Error: {s}\n", .{errorDescription(err)});

                return 1;
            };

            defer self.allocator.free(object_file_path);

            compilation.emit(object_file_path, options.output_kind) catch |err| {
                std.debug.print("Error: could not emit object file: {s}\n", .{errorDescription(err)});

                return 1;
            };

            if (options.output_kind == .executable) {
                const linker_exit_code = compilation.link(object_file_path, options.maybe_output_file_path orelse std.fs.path.stem(options.input_file_path)) catch |err| {
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
