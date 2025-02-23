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
    options.verbose = true;

    var repl = Repl.init(gpa.allocator());
    if (options.repl_start) {
        repl.start(&options);
    } else {
        runFile(&options);
    }
}

const Repl = struct {
    has_errors: bool,
    should_quit: bool,
    allocator: Allocator,

    fn init(allocator: Allocator) Repl {
        return .{
            .has_errors = false,
            .should_quit = false,
            .allocator = allocator,
        };
    }

    fn start(self: *Repl, options: *const opt.Options) void {
        const stdin = std.io.getStdIn().reader();

        debug.print("Lox interpreter in Zig\n", .{});
        debug.print("(Use -h to print out the available options)\n", .{});
        debug.print("(Verbose mode -v turned on during development)\n", .{});

        const buffer_size = 1024;
        var input_buffer: []u8 = undefined;
        while (!self.should_quit) {
            debug.print(">> ", .{});
            input_buffer = stdin.readUntilDelimiterAlloc(self.allocator, '\n', buffer_size) catch "";
            defer input_buffer = "";

            const input = std.mem.trim(u8, input_buffer, " \r");
            if (input.len == 0) {
                continue;
            } else {
                run(self.allocator, input, options);
            }
            defer self.allocator.free(input_buffer);
        }
    }
};

fn runFile(options: *const opt.Options) void {
    _ = options;
    debug.print("TODO(yemon): WIP on the runFile(..) function on the given script\n", .{});
}

fn run(allocator: Allocator, source: []const u8, options: *const opt.Options) void {
    var scanner = lexer.Scanner.init(allocator, source, options.verbose);
    scanner.startScanning();

    if (options.verbose) {
        for (scanner.tokens.items) |token| {
            token.display();
        }
    }

    // NOTE(yemon): `ParserError` is being printed out here temporarily.
    // Idealy, the parser should handle the error states internally, and 
    // shouldn't bubble up at all.
    var parser = Parser.init(&scanner.tokens);
    const expr = parser.parse(allocator) catch |err| {
        debug.print("Error when parsing the expression tree: {}\n", .{ err });
        return;
    };
    debug.print("AST result from the Parser:\n", .{});
    if (!parser.has_error) {
        expr.display(allocator, true);
    } else {
        debug.print("Parser internals seem to have some errors.\n", .{});
    }

    const value = interpreter.evaluate(expr, allocator) catch |err| {
        debug.print("Error evaluating the expression: {}\n", .{ err });
        return;
    };
    debug.print("Interpreter evaluated value:\n", .{});
    debug.print("{s}\n", .{ value.toString(allocator) });
}

fn reportError(line: u32, where: []const u8, message: []const u8) void {
    debug.print("[{}] ERR {s}: {s}\n", .{ line, where, message });
}
