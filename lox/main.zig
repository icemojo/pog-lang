const std       = @import("std");
const debug     = @import("std").debug;
const Allocator = @import("std").mem.Allocator;

const opt         = @import("options.zig");
const lexer       = @import("lexer.zig");
const ast         = @import("ast.zig");
const Parser      = @import("parser.zig").Parser;
const Interpreter = @import("interpreter.zig").Interpreter;

pub fn main() void {
    // NOTE(yemon): Maybe the repl could use an arena allocator, 
    // which can essentially reset after every execution.
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();
    // defer _ = gpa.deinit();

    var options = opt.parseOptions(allocator);
    defer {
        if (options.input_file_path) |input_file_path| {
            allocator.free(input_file_path);
        }
    }

    var interpreter = Interpreter.init(allocator);
    interpreter.debug_print = options.verbose;
    interpreter.debug_env = options.show_env;

    var repl = Repl.init(&interpreter);
    if (options.repl_start) {
        repl.start(allocator, &options);
    } else {
        runFile(allocator, &interpreter, &options);
    }
}

const Repl = struct {
    interpreter: *Interpreter,
    has_errors: bool,
    should_quit: bool,

    fn init(interpreter: *Interpreter) Repl {
        return .{
            .interpreter = interpreter,
            .has_errors = false,
            .should_quit = false,
        };
    }

    fn start(self: *Repl, allocator: Allocator, options: *const opt.Options) void {
        const stdin = std.io.getStdIn().reader();

        debug.print("Lox interpreter in Zig\n", .{});
        debug.print("(Use -h to print out the available options)\n", .{});
        if (options.verbose) {
            debug.print("(Verbose mode -v turned on.)\n", .{});
        }

        const buffer_size = 1024;
        var input_buffer: []u8 = undefined;
        while (!self.should_quit) {
            debug.print(">> ", .{});
            input_buffer = stdin.readUntilDelimiterAlloc(allocator, '\n', buffer_size) catch "";
            defer allocator.free(input_buffer);     // NOTE(yemon): is this really necessary, or working?

            const input = std.mem.trim(u8, input_buffer, " \r");
            if (input.len == 0) {
                continue;
            } else {
                run(allocator, self.interpreter, input, options);
            }
        }
    }
};

fn runFile(allocator: Allocator, interpreter: *Interpreter, options: *const opt.Options) void {
    if (options.*.input_file_path) |input_file_path| {
        debug.print("Input file path: {s}\n", .{ input_file_path[0..] });
        const contents = opt.openReadFile(allocator, input_file_path) catch |err| {
            debug.print("ERROR: {}\n", .{ err });
            return;
        };
        defer allocator.free(contents);

        debug.print("openReadFile(..) output ({} bytes):\n", .{ contents.len });
        // debug.print("{s}\n", .{ contents });

        run(allocator, interpreter, contents, options);
    } else {
        debug.print("No script files provided in the command line as the first argument.\n", .{});
    }
}

fn run(allocator: Allocator, interpreter: *Interpreter, source: []const u8, options: *const opt.Options) void {
    var scanner = lexer.Scanner.init(allocator, source, options.verbose);
    scanner.startScanning(options.repl_start);

    if (options.show_tokens) {
        for (scanner.tokens.items) |token| {
            token.display();
        }
    }

    var parser = Parser.init(&scanner.tokens);
    parser.debug_print = options.verbose;
    // NOTE(yemon): `ParserError` is being printed out here temporarily.
    // Idealy, the parser should handle the error states internally, and 
    // shouldn't bubble up at all.
    const statements = parser.parse(allocator) catch |err| {
        debug.print("Error when parsing the expression tree: {}\n", .{ err });
        return;
    };

    interpreter.executeAll(allocator, statements) catch |err| {
        debug.print("Runtime error occured:\n", .{});
        debug.print("{}\n", .{ err });
        return;
    };
}

fn reportError(line: u32, where: []const u8, message: []const u8) void {
    debug.print("[{}] ERR {s}: {s}\n", .{ line, where, message });
}
