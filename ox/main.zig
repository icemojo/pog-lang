const std = @import("std");
const debug = @import("std").debug;
const Allocator = @import("std").mem.Allocator;

const opt = @import("options.zig");
const Chunk = @import("chunk.zig");

pub fn main() void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();
    // defer _ = gpa.deinit();

    const options = opt.parseOptions(allocator);
    defer {
        if (options.input_file_path) |file_path| {
            allocator.free(file_path);
        }
    }

    const name = "Ox bytecode VM";
    debug.print(name ++ "\n", .{});
    debug.print("(Use -h to print out the available options)\n", .{});
    if (options.verbose) {
        debug.print("(Verbose mode -v turned on.)\n", .{});
    }

    var chunk = Chunk.init(allocator);
    defer chunk.deinit();

    chunk.writeCode(.op_add, 98);
    chunk.writeCode(.op_constant, 98);
    chunk.writeByte(chunk.addConstant(12), 98);
    chunk.writeCode(.op_constant, 98);
    chunk.writeByte(chunk.addConstant(2048), 98);
    chunk.writeCode(.op_return, 102);

    chunk.disassemble("Test chunk");
}
