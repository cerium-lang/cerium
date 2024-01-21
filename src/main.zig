const std = @import("std");
const Driver = @import("Driver.zig");

pub fn main() u8 {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};

    var arena = std.heap.ArenaAllocator.init(gpa.allocator());
    defer arena.deinit();

    const allocator = arena.allocator();

    var argiterator = std.process.ArgIterator.initWithAllocator(allocator) catch {
        std.debug.print("ran out of memory\n", .{});

        return 1;
    };
    defer argiterator.deinit();

    var driver = Driver.init(allocator);

    return driver.run(&argiterator);
}
