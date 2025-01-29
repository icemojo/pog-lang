const std = @import("std");

const Options = struct {
    verbose: bool,
    show_help: bool,
    tokenize_only: bool,
    repl_start: bool,
};

pub fn parseOptions() Options {
    const eql = std.mem.eql;
    const pa = std.heap.page_allocator;

    // NOTE(yemon): `unreachable` generates a panic in Debug and ReleaseSafe build modes.
    const args = std.process.argsAlloc(pa) catch unreachable;
    defer std.process.argsFree(pa, args);

    var options = Options{
        .verbose = false,
        .show_help = false,
        .tokenize_only = false,
        .repl_start = false,
    };
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
            options.tokenize_only = true;
            continue;
        }
        if (eql(u8, arg, "-r") or eql(u8, arg, "--repl")) {
            options.repl_start = true;
            continue;
        }
    }
    return options;
}
