const std = @import("std");
const debug = @import("std").debug;
const Allocator = @import("std").mem.Allocator;

pub const version = 0.1;

pub const CmdOptions = struct {
    verbose: bool,
    show_version: bool,
    show_help: bool,
    show_tokens: bool,
    debug_parser: bool,
    debug_ast: bool,
    show_env: bool,
    input_file_path: ?[]u8,

    const default: CmdOptions = .{
        .verbose = false,
        .show_version = false,
        .show_help = false,
        .show_tokens = false,
        .debug_parser = false,
        .debug_ast = false,
        .show_env = false,
        .input_file_path = null,
    };
};

pub fn parseOptions(allocator: Allocator) CmdOptions {
    const eql = std.mem.eql;

    // NOTE(yemon): `unreachable` generates a panic in Debug and ReleaseSafe build modes.
    const args = std.process.argsAlloc(allocator) catch unreachable;
    defer std.process.argsFree(allocator, args);

    var options: CmdOptions = .default;
    for (args[1..]) |arg| {
        if (eql(u8, arg, "--version")) {
            options.show_version = true;
            continue;
        }
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
        if (eql(u8, arg, "-p") or eql(u8, arg, "--parser")) {
            options.debug_parser = true;
            continue;
        }
        if (eql(u8, arg, "-a") or eql(u8, arg, "--ast")) {
            options.debug_ast = true;
            continue;
        }
        if (eql(u8, arg, "-e") or eql(u8, arg, "--env")) {
            options.show_env = true;
            continue;
        }

        options.input_file_path = allocator.alloc(u8, arg.len) catch unreachable;
        @memcpy(options.input_file_path.?, arg);
    }

    return options;
}

pub fn displayHelp() void {
    debug.print("Pog language interpreter (v{d})\n", .{ version });
    debug.print("\n", .{});

    debug.print("  Usage: pog [script_file] [options...]\n", .{});
    debug.print("\n", .{});

    displayOption(null, "--version", "Display the current version of the interpreter");
    displayOption("-h", "--help",    "Display this help prompt");
    displayOption("-v", "--verbose", "Turn on the verbose mode");
    debug.print("\n", .{});

    debug.print("  Trigging Pog without the [script_file] argument will kick start the REPL mode.\n\n", .{});

    debug.print("  Debug options:\n", .{});
    displayOption("-t", "--tokenize", "Display the output of the lexer as list of tokens");
    displayOption("-p", "--parser",   "Display the parser output log");
    displayOption("-a", "--ast",      "Display the AST output");
    displayOption("-e", "--env",      "Display the environment tracking variables in each call stack scope");
    debug.print("\n", .{});
}

fn displayOption(short: ?[]const u8, long: []const u8, message: []const u8) void {
    if (short) |it| {
        debug.print("  {s:4},  {s:<12}  {s}\n", .{ it, long, message });
    } else {
        debug.print("  {s:4}   {s:<12}  {s}\n", .{ "", long, message });
    }
}

pub fn openReadFile(allocator: Allocator, input_file_path: []const u8, verbose: bool) ![]u8 {
    const cwd = std.fs.cwd();
    const file = cwd.openFile(input_file_path, .{}) catch |err| {
        switch (err) {
            error.FileNotFound => {
                debug.print("The given script file \'{s}\' does not exist.\n", .{ input_file_path });
            },
            else => {
                debug.print("Error openeing file: {}\n", .{ err });
            }
        }
        return "";
    };
    defer file.close();

    const max_buffer = 1024 * 10 * 10;
    var buffer = try allocator.alloc(u8, max_buffer);
    defer allocator.free(buffer);

    try file.seekTo(0);
    const bytes_read = try file.readAll(buffer[0..]);
    if (verbose) {
        debug.print("Read {} bytes from source:\n", .{ bytes_read });
    }

    const contents = try allocator.alloc(u8, bytes_read);
    @memcpy(contents, buffer[0..bytes_read]);
    return contents;
}

