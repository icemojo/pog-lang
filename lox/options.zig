const std = @import("std");
const debug = @import("std").debug;
const Allocator = @import("std").mem.Allocator;

pub const Options = struct {
    verbose: bool,
    show_help: bool,
    show_tokens: bool,
    repl_start: bool,
};

pub fn parseOptions(allocator: Allocator) Options {
    const eql = std.mem.eql;

    // NOTE(yemon): `unreachable` generates a panic in Debug and ReleaseSafe build modes.
    const args = std.process.argsAlloc(allocator) catch unreachable;
    defer std.process.argsFree(allocator, args);

    var options = Options{
        .verbose = false,
        .show_help = false,
        .show_tokens = false,
        .repl_start = false,
    };
    var unknowns = std.ArrayList([]u8).init(allocator);
    for (args[1..]) |arg| {
        if (eql(u8, arg, "-v") or eql(u8, arg, "--verbose")) {
            options.verbose = true;
            continue;
        }
        if (eql(u8, arg, "-h") or eql(u8, arg, "--help")) {
            options.show_help = true;
            continue;
        }
        if (eql(u8, arg, "-t") or eql(u8, arg, "--tokenize")) {
            options.show_tokens = true;
            continue;
        }
        if (eql(u8, arg, "-r") or eql(u8, arg, "--repl")) {
            options.repl_start = true;
            continue;
        }
        unknowns.append(arg) catch continue;
    }
    if (unknowns.items.len > 0) {
        debug.print("Unknown list of command line arguments:", .{});
        for (unknowns.items) |arg| {
            debug.print(" {s}", .{arg});
        }
        debug.print("\n", .{});
    }
    return options;
}
