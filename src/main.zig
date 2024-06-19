const std = @import("std");
const Driver = @import("Driver.zig");

pub fn main() u8 {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};

    var arena_instance = std.heap.ArenaAllocator.init(gpa.allocator());
    defer arena_instance.deinit();

    const arena = arena_instance.allocator();

    var arg_iterator = std.process.ArgIterator.initWithAllocator(arena) catch {
        std.debug.print("ran out of memory\n", .{});

        return 1;
    };

    defer arg_iterator.deinit();

    var driver = Driver.init(arena);

    return driver.run(&arg_iterator);
}
