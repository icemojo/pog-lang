const std = @import("std");
const debug = @import("std").debug;
const log = @import("std").log;

const opt = @import("options.zig");

pub fn main() void {
    debug.print("Lox interpreter in Zig\n", .{});
    debug.print("(Verbose mode -v turned on during development)\n", .{});

    var options = opt.parseOptions();
    options.verbose = true;

    if (options.repl_start) {
        debug.print("WIP: Will initiate the repl...\n", .{});
    } else {
        runFile(&options);
    }
}

fn runFile(options: *opt.Options) void {
    _ = options;
    debug.print("TODO(yemon): WIP on the runFile(..) function on the given script\n", .{});
}
