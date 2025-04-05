const std = @import("std");
const debug = @import("std").debug;
const Allocator = @import("std").mem.Allocator;

const OpCode = enum(u8) {
    op_return = 0,
    op_constant,
    op_add,
};

const Byte = union(enum) {
    op_code: OpCode,
    byte: u8,
};

const Value = f64;

constants: std.ArrayList(Value),
codes: std.ArrayList(Byte),
lines: std.ArrayList(usize),

const Self = @This();

pub fn init(allocator: Allocator) Self {
    return .{
        .constants = std.ArrayList(Value).initCapacity(allocator, 8) catch unreachable,
        .codes = std.ArrayList(Byte).initCapacity(allocator, 8) catch unreachable,
        .lines = std.ArrayList(usize).initCapacity(allocator, 8) catch unreachable,
    };
}

pub fn deinit(self: *Self) void {
    self.constants.deinit();
    self.codes.deinit();
    self.lines.deinit();
}

pub fn writeByte(self: *Self, byte: u8, line: usize) void {
    self.codes.append(.{ .byte = byte }) catch unreachable;
    self.lines.append(line) catch unreachable;
}

pub fn writeCode(self: *Self, code: OpCode, line: usize) void {
    self.codes.append(.{ .op_code = code }) catch unreachable;
    self.lines.append(line) catch unreachable;
}

pub fn addConstant(self: *Self, value: Value) u8 {
    // TODO(yemon): the constants 'added position' is going to be stored as 
    // [OP_CONSTANT][index] as 2-bytes instruction.
    // So potentially, the size of the `len` should be 1 byte (u8).
    // Right now, it's just 'usize'.
    // NOTE(yemon): custom `ArrayList` type without downcasting??
    self.constants.append(value) catch unreachable;
    const pos: u8 = @intCast(self.constants.items.len-1);
    return pos;
}

pub fn disassemble(self: *const Self, comptime label: []const u8) void {
    const title = "== " ++ label ++ " ==";
    debug.print(title ++ "\n", .{});

    if (self.constants.items.len > 0) {
        for (self.constants.items, 0..) |constant, idx| {
            debug.print("[{}]{d} ", .{ idx, constant });
        }
    }
    debug.print("\n", .{});

    const sep = "-" ** title.len;
    debug.print(sep ++ "\n", .{});

    var offset: usize = 0;
    while (offset < self.codes.items.len) {
        const line = self.lines.items[offset];
        if (offset == 0) {
            debug.print("{:0>4} {:>4}  ", .{ offset, line });
        } else {
            const prev_line = self.lines.items[offset-1];
            if (line != prev_line) {
                debug.print("{:0>4} {:>4}  ", .{ offset, line });
            } else {
                debug.print("{:0>4} {c:>4}  ", .{ offset, '|' });
            }
        }

        offset = self.disassembleInstruction(offset);
        debug.print("\n", .{});
    }
    debug.print(sep ++ "\n", .{});
}

fn disassembleInstruction(self: *const Self, offset: usize) usize {
    const instruction: Byte = self.codes.items[offset];

    const next_offset: usize = display: switch (instruction) {
        .op_code => |code| {
            const size: usize = code_offset: switch (code) {
                .op_return => {
                    debug.print("OP_RETURN", .{});
                    break :code_offset 1;
                },
                .op_add => {
                    debug.print("OP_ADD", .{});
                    break :code_offset 1;
                },
                .op_constant => {
                    debug.print("OP_CONSTANT [", .{});
                    _ = self.disassembleInstruction(offset + 1);
                    debug.print("]", .{});
                    break :code_offset 2;
                },
            };
            break :display (offset + size);
        },

        .byte => |byte| {
            debug.print("{any}", .{ byte });
            break :display (offset + 1);
        },
    };

    return next_offset;
}
