const std = @import("std");
const builtin = @import("builtin");

const Compilation = @import("compiler/Compilation.zig");
const Air = @import("compiler/Air.zig");

pub const OutputKind = enum {
    assembly,
    object,
    executable,
    ir,
    none,
};

pub const RunnerKind = enum {
    executable,
    none,
};

pub const CodeModel = enum {
    default,
    tiny,
    small,
    kernel,
    medium,
    large,
};

pub const Cli = struct {
    allocator: std.mem.Allocator,

    program: []const u8,
    command: ?Command = null,

    const Command = union(enum) {
        compile: Compile,
        run: Run,
        help,

        const Compile = struct {
            root_file_path: []const u8,
            maybe_output_file_path: ?[]const u8,
            output_kind: OutputKind,
            runner_kind: RunnerKind,
            target: std.Target,
            code_model: CodeModel,

            const usage =
                \\Usage:
                \\  {s} compile <root-file-path> [options]
                \\
                \\Options:
                \\  --output <output-file-path>    -- specify the output file path
                \\  --emit <output-kind>           -- specify the output kind
                \\                                    [assembly, object, executable (default), ir, none]
                \\  --runner <runner-kind>         -- specify the runner kind
                \\                                    [executable (default), none]
                \\  --target <arch-os-abi>         -- specify the target query
                \\
                \\  --code-model <code-model>     -- specify the code model
                \\                                    [default, tiny, small, kernel, medium, large]
                \\
                \\
            ;
        };

        const Run = struct {
            root_file_path: []const u8,
            runner_kind: RunnerKind,
            target: std.Target,
            code_model: CodeModel,
            arguments: []const []const u8,

            const usage =
                \\Usage:
                \\  {s} run <root-file-path> [options] [-- [arguments]]
                \\
                \\Options:
                \\  --runner <runner-kind>         -- specify the runner kind
                \\                                    [executable (default), none]
                \\  --target <arch-os-abi>         -- specify the target query
                \\
                \\  --code-model <code-model>     -- specify the code model
                \\                                    [default, tiny, small, kernel, medium, large]
                \\
                \\
            ;
        };
    };

    const usage =
        \\Usage:
        \\  {s} <command> [options]
        \\
        \\Commands:
        \\  compile                       -- compile certain file
        \\  run                           -- compile certain file into an executable and run it
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

    fn parseRunnerKindOption(self: Cli, raw_runner_kind: []const u8) ?RunnerKind {
        if (std.mem.eql(u8, raw_runner_kind, "executable")) {
            return .executable;
        } else if (std.mem.eql(u8, raw_runner_kind, "none")) {
            return .none;
        } else {
            std.debug.print(Command.Compile.usage, .{self.program});

            std.debug.print("Error: unrecognized runner kind: {s}\n", .{raw_runner_kind});

            return null;
        }
    }

    fn parseOutputKindOption(self: Cli, raw_output_kind: []const u8) ?OutputKind {
        if (std.mem.eql(u8, raw_output_kind, "assembly")) {
            return .assembly;
        } else if (std.mem.eql(u8, raw_output_kind, "object")) {
            return .object;
        } else if (std.mem.eql(u8, raw_output_kind, "executable")) {
            return .executable;
        } else if (std.mem.eql(u8, raw_output_kind, "ir")) {
            return .ir;
        } else if (std.mem.eql(u8, raw_output_kind, "none")) {
            return .none;
        } else {
            std.debug.print(Command.Compile.usage, .{self.program});

            std.debug.print("Error: unrecognized output kind: {s}\n", .{raw_output_kind});

            return null;
        }
    }

    fn parseCodeModelOption(self: Cli, raw_code_model: []const u8) ?CodeModel {
        if (std.mem.eql(u8, raw_code_model, "default")) {
            return .default;
        } else if (std.mem.eql(u8, raw_code_model, "tiny")) {
            return .tiny;
        } else if (std.mem.eql(u8, raw_code_model, "small")) {
            return .small;
        } else if (std.mem.eql(u8, raw_code_model, "kernel")) {
            return .kernel;
        } else if (std.mem.eql(u8, raw_code_model, "medium")) {
            return .medium;
        } else if (std.mem.eql(u8, raw_code_model, "large")) {
            return .large;
        } else {
            std.debug.print(Command.Compile.usage, .{self.program});

            std.debug.print("Error: unrecognized code model: {s}\n", .{raw_code_model});

            return null;
        }
    }

    fn parseTargetQueryOption(raw_target_query: []const u8) ?std.Target {
        const target_query = std.Target.Query.parse(.{ .arch_os_abi = raw_target_query }) catch |err| {
            std.debug.print("Error: could not parse target query: {s}\n", .{errorDescription(err)});

            return null;
        };

        return std.zig.system.resolveTargetQuery(target_query) catch |err| {
            std.debug.print("Error: could not resolve target query: {s}\n", .{errorDescription(err)});

            return null;
        };
    }

    fn parse(allocator: std.mem.Allocator, argument_iterator: *std.process.ArgIterator) ?Cli {
        var self: Cli = .{
            .allocator = allocator,
            .program = argument_iterator.next().?,
        };

        while (argument_iterator.next()) |argument| {
            if (std.mem.eql(u8, argument, "compile")) {
                const root_file_path = argument_iterator.next() orelse {
                    std.debug.print(Command.Compile.usage, .{self.program});

                    std.debug.print("Error: expected input file path\n", .{});

                    return null;
                };

                var maybe_output_file_path: ?[]const u8 = null;
                var output_kind: OutputKind = .executable;
                var runner_kind: RunnerKind = .executable;
                var target: std.Target = builtin.target;
                var code_model: CodeModel = .default;

                while (argument_iterator.next()) |next_argument| {
                    if (std.mem.eql(u8, next_argument, "--output")) {
                        maybe_output_file_path = argument_iterator.next() orelse {
                            std.debug.print(Command.Compile.usage, .{self.program});

                            std.debug.print("Error: expected output file path\n", .{});

                            return null;
                        };
                    } else if (std.mem.eql(u8, next_argument, "--emit")) {
                        if (argument_iterator.next()) |raw_output_kind| {
                            output_kind = self.parseOutputKindOption(raw_output_kind) orelse return null;
                        } else {
                            std.debug.print(Command.Compile.usage, .{self.program});

                            std.debug.print("Error: expected output kind\n", .{});

                            return null;
                        }
                    } else if (std.mem.eql(u8, next_argument, "--runner")) {
                        if (argument_iterator.next()) |raw_runner_kind| {
                            runner_kind = self.parseRunnerKindOption(raw_runner_kind) orelse return null;
                        } else {
                            std.debug.print(Command.Compile.usage, .{self.program});

                            std.debug.print("Error: expected runner kind\n", .{});

                            return null;
                        }
                    } else if (std.mem.eql(u8, next_argument, "--target")) {
                        if (argument_iterator.next()) |raw_target_query| {
                            target = parseTargetQueryOption(raw_target_query) orelse return null;
                        } else {
                            std.debug.print(Command.Compile.usage, .{self.program});

                            std.debug.print("Error: expected target query\n", .{});

                            return null;
                        }
                    } else if (std.mem.eql(u8, next_argument, "--code-model")) {
                        if (argument_iterator.next()) |raw_code_model| {
                            code_model = self.parseCodeModelOption(raw_code_model) orelse return null;
                        } else {
                            std.debug.print(Command.Compile.usage, .{self.program});

                            std.debug.print("Error: expected code model\n", .{});

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
                        .root_file_path = root_file_path,
                        .maybe_output_file_path = maybe_output_file_path,
                        .output_kind = output_kind,
                        .runner_kind = runner_kind,
                        .target = target,
                        .code_model = code_model,
                    },
                };
            } else if (std.mem.eql(u8, argument, "run")) {
                const root_file_path = argument_iterator.next() orelse {
                    std.debug.print(Command.Run.usage, .{self.program});

                    std.debug.print("Error: expected input file path\n", .{});

                    return null;
                };

                var runner_kind: RunnerKind = .executable;
                var target: std.Target = builtin.target;
                var code_model: CodeModel = .default;

                while (argument_iterator.next()) |next_argument| {
                    if (std.mem.eql(u8, next_argument, "--runner")) {
                        if (argument_iterator.next()) |raw_runner_kind| {
                            runner_kind = self.parseRunnerKindOption(raw_runner_kind) orelse return null;
                        } else {
                            std.debug.print(Command.Run.usage, .{self.program});

                            std.debug.print("Error: expected runner kind\n", .{});

                            return null;
                        }
                    } else if (std.mem.eql(u8, next_argument, "--target")) {
                        if (argument_iterator.next()) |raw_target_query| {
                            target = parseTargetQueryOption(raw_target_query) orelse return null;
                        } else {
                            std.debug.print(Command.Run.usage, .{self.program});

                            std.debug.print("Error: expected target query\n", .{});

                            return null;
                        }
                    } else if (std.mem.eql(u8, next_argument, "--code-model")) {
                        if (argument_iterator.next()) |raw_code_model| {
                            code_model = self.parseCodeModelOption(raw_code_model) orelse return null;
                        } else {
                            std.debug.print(Command.Compile.usage, .{self.program});

                            std.debug.print("Error: expected code model\n", .{});

                            return null;
                        }
                    } else if (std.mem.eql(u8, next_argument, "--")) {
                        var remaining_arguments: std.ArrayListUnmanaged([]const u8) = .{};

                        while (argument_iterator.next()) |remaining_argument| {
                            remaining_arguments.append(self.allocator, remaining_argument) catch |err| {
                                std.debug.print("Error: {s}\n", .{errorDescription(err)});

                                return null;
                            };
                        }

                        self.command = .{
                            .run = .{
                                .root_file_path = root_file_path,
                                .runner_kind = runner_kind,
                                .target = target,
                                .code_model = code_model,
                                .arguments = remaining_arguments.toOwnedSlice(self.allocator) catch |err| {
                                    std.debug.print("Error: {s}\n", .{errorDescription(err)});

                                    return null;
                                },
                            },
                        };

                        return self;
                    }
                }

                self.command = .{
                    .run = .{
                        .root_file_path = root_file_path,
                        .runner_kind = runner_kind,
                        .target = target,
                        .code_model = code_model,
                        .arguments = &.{},
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

    fn compileStep(
        self: Cli,
        root_file_path: []const u8,
        maybe_output_file_path: ?[]const u8,
        output_kind: OutputKind,
        runner_kind: RunnerKind,
        target: std.Target,
        code_model: CodeModel,
    ) u8 {
        const cerium_lib_dir = Compilation.Environment.openCeriumLibrary() catch {
            std.debug.print("Error: could not open the cerium library directory\n", .{});

            return 1;
        };

        const root_file = std.fs.cwd().openFile(root_file_path, .{}) catch |err| {
            std.debug.print("Error: could not open root file '{s}': {s}\n", .{ root_file_path, errorDescription(err) });

            return 1;
        };

        const root_file_buffer = root_file.readToEndAllocOptions(self.allocator, std.math.maxInt(u32), null, @alignOf(u8), 0) catch |err| {
            std.debug.print("Error: could not read root file '{s}': {s}\n", .{ root_file_path, errorDescription(err) });

            return 1;
        };

        var compilation = Compilation.init(
            self.allocator,
            .{ .path = root_file_path, .buffer = root_file_buffer },
            .{
                .cerium_lib_dir = cerium_lib_dir,
                .target = target,
            },
        );

        const compilation_file: Compilation.File = switch (runner_kind) {
            .executable => blk: {
                const relative_runner_file_path = "std" ++ std.fs.path.sep_str ++ "runner" ++ std.fs.path.sep_str ++ "exe.cerm";

                var runner_file_path_buffer: [std.fs.max_path_bytes]u8 = undefined;

                const runner_file_path = cerium_lib_dir.realpath(relative_runner_file_path, &runner_file_path_buffer) catch |err| {
                    std.debug.print("Error: could not find executable runner file path: {s}\n", .{errorDescription(err)});

                    return 1;
                };

                const runner_file = std.fs.cwd().openFile(runner_file_path, .{}) catch |err| {
                    std.debug.print("Error: could not open executable runner file: {s}\n", .{errorDescription(err)});

                    return 1;
                };

                const runner_file_buffer = runner_file.readToEndAllocOptions(self.allocator, std.math.maxInt(u32), null, @alignOf(u8), 0) catch |err| {
                    std.debug.print("Error: could not read executable runner file: {s}\n", .{errorDescription(err)});

                    return 1;
                };

                break :blk .{ .path = runner_file_path, .buffer = runner_file_buffer };
            },

            .none => .{ .path = root_file_path, .buffer = root_file_buffer },
        };

        var sir = compilation.parse(compilation_file) orelse return 1;

        const airs = compilation.analyze(compilation_file, sir) orelse return 1;

        sir.instructions.deinit(self.allocator);

        switch (output_kind) {
            .assembly => {
                const assembly_file_path = std.fmt.allocPrintZ(self.allocator, "{s}{s}", .{
                    maybe_output_file_path orelse std.fs.path.stem(root_file_path),
                    if (maybe_output_file_path != null) "" else ".s",
                }) catch |err| {
                    std.debug.print("Error: {s}\n", .{errorDescription(err)});

                    return 1;
                };

                defer self.allocator.free(assembly_file_path);

                compilation.emit(airs, assembly_file_path, output_kind, code_model) catch |err| {
                    std.debug.print("Error: could not emit assembly file: {s}\n", .{errorDescription(err)});

                    return 1;
                };

                self.allocator.free(airs);
            },

            .object, .executable => {
                const object_file_path = std.fmt.allocPrintZ(self.allocator, "{s}{s}", .{
                    maybe_output_file_path orelse std.fs.path.stem(root_file_path),
                    if (maybe_output_file_path != null and output_kind == .object) "" else target.ofmt.fileExt(target.cpu.arch),
                }) catch |err| {
                    std.debug.print("Error: {s}\n", .{errorDescription(err)});

                    return 1;
                };

                defer self.allocator.free(object_file_path);

                compilation.emit(airs, object_file_path, output_kind, code_model) catch |err| {
                    std.debug.print("Error: could not emit object file: {s}\n", .{errorDescription(err)});

                    return 1;
                };

                self.allocator.free(airs);

                if (output_kind == .executable) {
                    const linker_exit_code = compilation.link(
                        object_file_path,
                        maybe_output_file_path orelse std.fs.path.stem(root_file_path),
                    ) catch |err| {
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
            },

            .ir => {
                const ir_file_path = std.fmt.allocPrintZ(self.allocator, "{s}{s}", .{
                    maybe_output_file_path orelse std.fs.path.stem(root_file_path),
                    if (maybe_output_file_path != null and output_kind == .object) "" else ".air",
                }) catch |err| {
                    std.debug.print("Error: {s}\n", .{errorDescription(err)});

                    return 1;
                };

                defer self.allocator.free(ir_file_path);

                const ir_file = std.fs.cwd().createFile(ir_file_path, .{}) catch |err| {
                    std.debug.print("Error: could not create intermediate representation file: {s}\n", .{errorDescription(err)});

                    return 1;
                };

                Air.passes.format.print(ir_file.writer(), airs) catch |err| {
                    std.debug.print("Error: could not emit intermediate representation: {s}\n", .{errorDescription(err)});

                    return 1;
                };
            },

            .none => {},
        }

        return 0;
    }

    fn executeCompileCommand(self: Cli) u8 {
        const options = self.command.?.compile;

        return self.compileStep(
            options.root_file_path,
            options.maybe_output_file_path,
            options.output_kind,
            options.runner_kind,
            options.target,
            options.code_model,
        );
    }

    fn executeRunCommand(self: Cli) u8 {
        const options = self.command.?.run;

        const output_file_path = std.fs.path.stem(options.root_file_path);

        const compile_step_exit_code = self.compileStep(
            options.root_file_path,
            output_file_path,
            .executable,
            options.runner_kind,
            options.target,
            options.code_model,
        );

        if (compile_step_exit_code != 0) return compile_step_exit_code;

        const real_output_file_path = std.fs.realpathAlloc(self.allocator, output_file_path) catch |err| {
            std.debug.print("Error: could not resolve output file path: {s}\n", .{errorDescription(err)});

            return 1;
        };

        defer self.allocator.free(real_output_file_path);

        const exe_process_argv = std.mem.concat(self.allocator, []const u8, &.{
            &.{real_output_file_path},
            options.arguments,
        }) catch |err| {
            std.debug.print("Error: {s}\n", .{errorDescription(err)});

            return 1;
        };

        defer self.allocator.free(exe_process_argv);

        var exe_process = std.process.Child.init(exe_process_argv, self.allocator);
        exe_process.stdin_behavior = .Inherit;
        exe_process.stdout_behavior = .Inherit;
        exe_process.stderr_behavior = .Inherit;

        const termination = exe_process.spawnAndWait() catch |err| {
            std.debug.print("Error: could not run executable: {s}\n", .{errorDescription(err)});

            return 1;
        };

        std.fs.cwd().deleteFile(output_file_path) catch |err| {
            std.debug.print("Error: could not delete output file: {s}\n", .{errorDescription(err)});

            return 1;
        };

        switch (termination) {
            .Exited => |code| return code,

            else => return 1,
        }
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
        .run => return cli.executeRunCommand(),
        .help => return cli.executeHelpCommand(),
    }
}
