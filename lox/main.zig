const std = @import("std");
const debug = @import("std").debug;
const log = @import("std").log;
const Allocator = @import("std").mem.Allocator;

const opt = @import("options.zig");

pub fn main() void {
    var options = opt.parseOptions();
    options.verbose = true;

    var pa = std.heap.page_allocator;
    var repl = Repl{
        .has_errors = false,
        .should_quit = false,
    };
    if (options.repl_start) {
        repl.start(&pa, &options);
    } else {
        runFile(&options);
    }
}

const Repl = struct {
    has_errors: bool,
    should_quit: bool,

    fn start(self: *Repl, allocator: *Allocator, options: *const opt.Options) void {
        const stdin = std.io.getStdIn().reader();
        _ = options;
        debug.print("Lox interpreter in Zig\n", .{});
        debug.print("(Use -h to print out the available options)\n", .{});
        debug.print("(Verbose mode -v turned on during development)\n", .{});

        var input_buffer: []u8 = undefined;
        const buffer_size = 1024;
        while (!self.should_quit) {
            debug.print(">> ", .{});
            input_buffer = stdin.readUntilDelimiterAlloc(allocator.*, '\n', buffer_size) catch "";
            defer input_buffer = "";

            const input = std.mem.trim(u8, input_buffer, " \r");
            if (input.len == 0) {
                continue;
            } else {
                run(input);
            }
        }
        defer allocator.free(input_buffer);
    }
};

fn runFile(options: *const opt.Options) void {
    _ = options;
    debug.print("TODO(yemon): WIP on the runFile(..) function on the given script\n", .{});
}

fn run(source: []const u8) void {
    debug.print("Well run: {s}\n", .{source});
}
