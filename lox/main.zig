const std       = @import("std");
const debug     = @import("std").debug;
const Allocator = @import("std").mem.Allocator;

const opt         = @import("options.zig");
const lexer       = @import("lexer.zig");
const ast         = @import("ast.zig");
const Parser      = @import("parser.zig").Parser;
const interpreter = @import("interpreter.zig");

pub fn main() void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    var options = opt.parseOptions(gpa.allocator());

    // NOTE(yemon): Maybe the repl could use an arena allocator, 
    // which can essentially reset after every execution.
    const allocator = gpa.allocator();
    var repl = Repl.init();
    if (options.repl_start) {
        repl.start(allocator, &options);
    } else {
        runFile(allocator, &options);
    }
}

const Repl = struct {
    has_errors: bool,
    should_quit: bool,

    fn init() Repl {
        return .{
            .has_errors = false,
            .should_quit = false,
        };
    }

    fn start(self: *Repl, allocator: Allocator, options: *const opt.Options) void {
        const stdin = std.io.getStdIn().reader();

        debug.print("Lox interpreter in Zig\n", .{});
        debug.print("(Use -h to print out the available options)\n", .{});
        debug.print("(Verbose mode -v turned on during development)\n", .{});

        const buffer_size = 1024;
        var input_buffer: []u8 = undefined;
        while (!self.should_quit) {
            debug.print(">> ", .{});
            input_buffer = stdin.readUntilDelimiterAlloc(allocator, '\n', buffer_size) catch "";
            defer input_buffer = "";

            const input = std.mem.trim(u8, input_buffer, " \r");
            if (input.len == 0) {
                continue;
            } else {
                run(allocator, input, options);
            }
            defer allocator.free(input_buffer);
        }
    }
};

fn runFile(allocator: Allocator, options: *const opt.Options) void {
    _ = allocator;
    _ = options;
    debug.print("TODO(yemon): WIP on the runFile(..) function on the given script\n", .{});
}

fn run(allocator: Allocator, source: []const u8, options: *const opt.Options) void {
    var scanner = lexer.Scanner.init(allocator, source, options.verbose);
    scanner.startScanning();

    if (options.show_tokens) {
        for (scanner.tokens.items) |token| {
            token.display();
        }
    }

    var parser = Parser.init(&scanner.tokens);
    if (options.verbose) {
        parser.debug_print = true;
    }
    // NOTE(yemon): `ParserError` is being printed out here temporarily.
    // Idealy, the parser should handle the error states internally, and 
    // shouldn't bubble up at all.
    const statements = parser.parse(allocator) catch |err| {
        debug.print("Error when parsing the expression tree: {}\n", .{ err });
        return;
    };

    interpreter.execute(allocator, statements) catch |err| {
        debug.print("Runtime error occured:\n", .{});
        debug.print("{}\n", .{ err });
        return;
    };
}

fn reportError(line: u32, where: []const u8, message: []const u8) void {
    debug.print("[{}] ERR {s}: {s}\n", .{ line, where, message });
}
