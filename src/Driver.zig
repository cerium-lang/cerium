const std = @import("std");
const builtin = @import("builtin");
const ast = @import("compiler/ast.zig");
const CodeGen = @import("compiler/CodeGen.zig");
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
    \\      {s} <command> [arguments]
    \\
    \\Commands:
    \\      compile <file_path>     -- compile certain file
    \\
    \\
;

const compile_command_usage =
    \\Usage:
    \\      {s} compile <file_path>
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

    const in_file = std.fs.cwd().openFile(options.file_path, .{}) catch |err| {
        std.debug.print("{s}: {s}\n", .{ options.file_path, errorDescription(err) });

        return 1;
    };
    defer in_file.close();

    const in_file_content = in_file.reader().readAllAlloc(self.gpa, std.math.maxInt(u32)) catch |err| {
        std.debug.print("{s}: {s}\n", .{ options.file_path, errorDescription(err) });

        return 1;
    };
    defer self.gpa.free(in_file_content);

    var in_file_content_z = @as([:0]u8, @ptrCast(in_file_content));
    in_file_content_z[in_file_content_z.len] = 0;

    var parser = ast.Parser.init(self.gpa, in_file_content_z) catch |err| {
        std.debug.print("{s}\n", .{errorDescription(err)});

        return 1;
    };

    const root = parser.parseRoot() catch |err| switch (err) {
        error.UnexpectedToken => {
            std.debug.print("{s}:{}:{}: unexpected token\n", .{ options.file_path, parser.tokenLoc(parser.peekToken()).line, parser.tokenLoc(parser.peekToken()).column });

            return 1;
        },

        error.InvalidChar => {
            std.debug.print("{s}:{}:{}: invalid char\n", .{ options.file_path, parser.tokenLoc(parser.peekToken()).line, parser.tokenLoc(parser.peekToken()).column });

            return 1;
        },

        error.InvalidNumber => {
            std.debug.print("{s}:{}:{}: invalid number\n", .{ options.file_path, parser.tokenLoc(parser.peekToken()).line, parser.tokenLoc(parser.peekToken()).column });

            return 1;
        },

        error.InvalidType => {
            std.debug.print("{s}:{}:{}: invalid type\n", .{ options.file_path, parser.tokenLoc(parser.peekToken()).line, parser.tokenLoc(parser.peekToken()).column });

            return 1;
        },

        error.ExpectedTopLevelDeclaration => {
            std.debug.print("{s}:{}:{}: expected top level declaration\n", .{ options.file_path, parser.tokenLoc(parser.peekToken()).line, parser.tokenLoc(parser.peekToken()).column });

            return 1;
        },

        else => {
            std.debug.print("{s}\n", .{errorDescription(err)});

            return 1;
        },
    };

    var codegen = CodeGen.init(self.gpa);

    const ir = codegen.gen(root) catch |err| switch (err) {
        error.MismatchedTypes, error.UnexpectedReturn, error.ExpectedReturn => {
            std.debug.print("{s}:{}:{}: {s}\n", .{ options.file_path, codegen.error_info.?.loc.line, codegen.error_info.?.loc.column, codegen.error_info.?.message });

            return 1;
        },

        else => {
            std.debug.print("{s}\n", .{errorDescription(err)});

            return 1;
        },
    };

    const out_assembly_code = ir.render(self.gpa, builtin.target) catch |err| switch (err) {
        error.UnsupportedTarget => {
            std.debug.print("{s} is unspported yet\n", .{builtin.target.cpu.arch.genericName()});

            return 1;
        },

        else => {
            std.debug.print("{s}\n", .{errorDescription(err)});

            return 1;
        },
    };

    const out_assembly_file = std.fs.cwd().createFile("a.out.s", .{}) catch |err| {
        std.debug.print("couldn't create output assembly file: {s}\n", .{errorDescription(err)});

        return 1;
    };
    defer out_assembly_file.close();

    out_assembly_file.writer().writeAll(out_assembly_code) catch |err| {
        std.debug.print("couldn't write the output assembly code: {s}", .{errorDescription(err)});

        return 1;
    };

    return 0;
}
