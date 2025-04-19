const std = @import("std");
const debug = @import("std").debug;
const Allocator = @import("std").mem.Allocator;

const opt = @import("options.zig");
const VM = @import("vm.zig");
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

    const chunk = allocator.create(Chunk) catch unreachable;
    chunk.* = Chunk.init(allocator);
    var vm = VM.init(chunk);
    defer vm.deinit();

    vm.chunk.writeCode(.op_add, 98);
    vm.chunk.writeCode(.op_constant, 98);
    vm.chunk.writeByte(vm.chunk.addConstant(12), 98);
    vm.chunk.writeCode(.op_constant, 98);
    vm.chunk.writeByte(vm.chunk.addConstant(2048), 98);
    vm.chunk.writeCode(.op_constant, 104);
    vm.chunk.writeByte(vm.chunk.addConstant(3.141592), 104);
    vm.chunk.writeCode(.op_return, 105);

    vm.chunk.disassemble("Test chunk");

    const result = vm.interpret();
    _ = result;
}
