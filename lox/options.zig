const std = @import("std");
const debug = @import("std").debug;
const Allocator = @import("std").mem.Allocator;

pub const Options = struct {
    verbose: bool,
    show_help: bool,
    show_tokens: bool,
    show_env: bool,
    repl_start: bool,
    input_file_path: ?[]u8,
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
        .show_env = false,
        .repl_start = false,
        .input_file_path = null,
    };
    var unknowns = std.ArrayList([]u8).init(allocator);
    for (args[1..], 0..) |arg, idx| {
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
        if (eql(u8, arg, "-e") or eql(u8, arg, "--env")) {
            options.show_env = true;
            continue;
        }
        if (eql(u8, arg, "-r") or eql(u8, arg, "--repl")) {
            options.repl_start = true;
            continue;
        }
        if (idx == 0) read_input: {
            options.input_file_path = allocator.alloc(u8, arg.len) 
                catch break :read_input;
            @memcpy(options.input_file_path.?, arg);
        } else {
            unknowns.append(arg) catch continue;
        }
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

pub fn openReadFile(allocator: Allocator, input_file_path: []const u8) ![]u8 {
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
    debug.print("Read {} bytes from source:\n", .{ bytes_read });

    const result = try allocator.alloc(u8, bytes_read);
    @memcpy(result, buffer[0..bytes_read]);
    return result;
}

pub fn openReadFileProperQuestionMark(allocator: Allocator, input_file_path: []const u8) !void {
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
        return;
    };
    defer file.close();

    const max_buffer = 1024 * 10 * 10;

    // const contents = file.readToEndAlloc(allocator, max_buffer)
    const contents = file.reader().readAllAlloc(allocator, max_buffer) 
        catch |err| 
    read: {
        debug.print("Error reading file contents: {}\n", .{ err });
        break :read null;
    };
    defer allocator.free(contents);

    debug.print("File contents:\n", .{});
    if (contents) |it| {
        debug.print("{s}\n", .{ it });
    }
    // debug.print("{s}\n", .{ buffer[0..bytes_read] });
}
