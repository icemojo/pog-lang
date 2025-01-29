const std = @import("std");
const options = @import("options.zig");

pub fn main() !void {
    const stdout = std.io.getStdOut().writer();
    try stdout.writeAll("Lox interpreter in Zig\n");
    try stdout.writeAll("(Verbose mode -v turned on during development)\n");

    var opt = try options.parseOptions();
    opt.verbose = true;
    try stdout.print("Options: {}\n", .{opt});
}
