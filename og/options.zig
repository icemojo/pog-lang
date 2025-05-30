const std = @import("std");
const Allocator = @import("std").mem.Allocator;

pub const Options = struct {
    verbose: bool,
    show_help: bool,
    input_file_path: ?[]u8,
};

pub fn parseOptions(allocator: Allocator) Options {
    const eql = std.mem.eql;

    var options = Options{
        .verbose = false,
        .show_help = false,
        .input_file_path = null,
    };
    const args = std.process.argsAlloc(allocator) catch 
        return options;
    defer std.process.argsFree(allocator, args);

    for (args[1..], 0..) |arg, idx| {
        if (eql(u8, arg, "-v") or eql(u8, arg, "--verbose")) {
            options.verbose = true;
            continue;
        }
        if (eql(u8, arg, "-h") or eql(u8, arg, "--help")) {
            options.show_help = true;
            continue;
        }

        // TODO(yemon): Maybe the input file path should/would not always be
        // the very first command line argument...
        if (idx == 0) read_input: {
            options.input_file_path = allocator.alloc(u8, arg.len)
                catch break :read_input;
            @memcpy(options.input_file_path.?, arg);
        }
    } 

    return options;
}
