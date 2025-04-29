const std       = @import("std");
const debug     = @import("std").debug;
const Allocator = @import("std").mem.Allocator;

const opt         = @import("options.zig");
const lexer       = @import("lexer.zig");
const ast         = @import("ast.zig");
const Parser      = @import("parser.zig");
const Interpreter = @import("interpreter.zig");

pub fn main() void {
    // NOTE(yemon): Maybe the repl could use an arena allocator, 
    // which can essentially reset after every execution.
    // var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    // const allocator = gpa.allocator();
    // defer _ = gpa.deinit();

    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();
 
    var options = opt.parseOptions(allocator);
    defer {
        if (options.input_file_path) |input_file_path| {
            allocator.free(input_file_path);
        }
    }

    if (options.show_help) {
        opt.displayHelp();
        return;
    }

    var interpreter = Interpreter.init(allocator);
    interpreter.debug_print = options.verbose;
    interpreter.debug_env = options.show_env;
    // defer allocator.destroy(interpreter.env);

    var repl = Repl.init(&interpreter);
    if (options.repl_start) {
        repl.start(allocator, &options);
    } else {
        if (options.input_file_path) |input_file_path| {
            runFile(allocator, &interpreter, input_file_path, &options);
        } else {
            debug.print("No script files provided in the command line as the first argument.\n", .{});
            debug.print("(Use -h to print out the available options)\n", .{});
        }
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

        debug.print("Lox interpreter implementation.\n", .{});
        debug.print("(Use -h to print out the available options)\n", .{});
        if (options.verbose) {
            debug.print("(Verbose mode -v turned on.)\n", .{});
        }

        // NOTE(yemon): Generally, the REPL should be working within the bounds
        // of an area allocator, both for parsing and evaluating.
        const buffer_size = 1024;
        var input_buffer: []u8 = undefined;
        while (!self.should_quit) {
            debug.print(">> ", .{});
            input_buffer = stdin.readUntilDelimiterAlloc(allocator, '\n', buffer_size) catch "";
            // NOTE(yemon): is this really necessary, or working?
            defer allocator.free(input_buffer);

            const input = std.mem.trim(u8, input_buffer, " \r");
            if (input.len == 0) {
                continue;
            } else {
                run(allocator, self.interpreter, input, options);
            }
        }
    }
};

fn runFile(
    allocator: Allocator, 
    interpreter: *Interpreter, 
    input_file_path: []const u8,
    options: *const opt.Options,
) void {
    if (options.verbose) {
        debug.print("Input file path: {s}\n", .{ input_file_path[0..] });
    }
    const contents = opt.openReadFile(allocator, input_file_path, options.verbose) 
        catch |err| {
            debug.print("ERROR: {}\n", .{ err });
            return;
        };
    defer allocator.free(contents);

    if (options.verbose) {
        debug.print("openReadFile(..) output ({} bytes):\n", .{ contents.len });
    }

    run(allocator, interpreter, contents, options);
}

fn run(
    allocator: Allocator, 
    interpreter: *Interpreter, 
    source: []const u8, 
    options: *const opt.Options,
) void {
    if (options.verbose) {
        debug.print("------------------------------------------------------------\n", .{});
    }
    var scanner = lexer.Scanner.init(allocator, source, options.show_tokens);
    scanner.startScanning(options.repl_start);

    if (options.show_tokens) {
        for (scanner.tokens.items) |token| {
            token.display();
        }
    }
    if (options.verbose) {
        debug.print("------------------------------------------------------------\n", .{});
    }

    var parser = Parser.init(&scanner.tokens, options.debug_parser, options.debug_ast);
    // NOTE(yemon): `ParserError` is being printed out here temporarily.
    // Idealy, the parser should handle the error states internally, and 
    // shouldn't bubble up at all.
    // NOTE(yemon): Parser errors can potentially prevent the interpreter execution
    // after the error has been reported (even with the errors that can be synchronized)
    const statements = parser.parse(allocator) catch |err| {
        debug.print("Error when parsing the expression tree: {}\n", .{ err });
        return;
    };
    if (options.verbose) {
        debug.print("------------------------------------------------------------\n", .{});
    }

    _ = interpreter.*.executeBlock(allocator, statements);
}
