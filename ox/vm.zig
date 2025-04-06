const std = @import("std");
const debug = @import("std").debug;
const Allocator = @import("std").mem.Allocator;

const Chunk = @import("chunk.zig");
const Byte = @import("chunk.zig").Byte;
const Value = @import("chunk.zig").Value;

chunk: *Chunk,
/// "instruction pointer" is sometimes known as 
/// "program counter (PC)" in some processor architectures
// NOTE(yemon): Would multi-pointer into the array be faster then the index access?
// But, `ip` as the local variable inside the actual interpretation function, would
// potentially be faster, because the compiler can probably keep the `ip` pointer in 
// a register as part of the stack frame.
ip: [*]Byte,

const Self = @This();

pub fn init(chunk: *Chunk) Self {
    return .{
        .chunk = chunk,
        .ip = chunk.codes.items.ptr,
    };
}

pub fn deinit(self: Self) void {
    self.chunk.deinit();
}

const InterpretResult = enum{
    ok,
    compile_error,
    runtime_error,
};

pub fn interpret(self: *Self) InterpretResult {
    const result: InterpretResult = run: while (true) : (self.ip += 1) {
        const instruction = self.ip[0];
        switch (instruction) {
            .op_code => |op_code| {
                switch (op_code) {
                    .op_return => {
                        break :run .ok;
                    },
                    .op_constant => {
                        if (self.readConstant()) |const_value| {
                            debug.print("{d}\n", .{ const_value });
                            continue :run;
                        } else {
                            // TODO(yemon): report runtime error in place!
                            break :run .runtime_error;
                        }
                    },
                    .op_add => {
                        // do nothing yet with add instruction
                        continue :run;
                    },
                }
            },
            .byte => {
                // do nothing yet with the data
                continue :run;
            },
        }
    };

    return result;
}

fn readConstant(self: *Self) ?Value {
    self.ip += 1;
    const next_instruction = self.ip[0];
    if (next_instruction.readByte()) |byte| {
        return self.chunk.constants.items[byte];
    } else {
        return null;
    }
}
