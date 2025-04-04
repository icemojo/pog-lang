const std = @import("std");
const debug = @import("std").debug;
const Allocator = @import("std").mem.Allocator;

pub const OpCode = enum(u8) {
    op_return = 0,
    op_constant,

    fn displayOffset(self: OpCode) usize {
        switch (self) {
            .op_return => {
                debug.print("OP_RETURN", .{});
                return 1;
            },
            .op_constant => {
                debug.print("OP_CONSTANT", .{});
                return 1;
            },
        }
        return 0;
    }
};


pub const Byte = union(enum) {
    op_code: OpCode,
    byte: u8,
};

pub const Value = f64;

fn printValue(self: Value) void {
    _ = self;
    debug.print("", .{});
}

codes: std.ArrayList(Byte),
constants: std.ArrayList(Value),

const Self = @This();

pub fn init(allocator: Allocator) Self {
    return .{
        .codes = std.ArrayList(Byte).initCapacity(allocator, 8) catch unreachable,
        .constants = std.ArrayList(Value).initCapacity(allocator, 8) catch unreachable,
    };
}

pub fn deinit(self: *Self) void {
    self.codes.deinit();
    self.constants.deinit();
}

pub fn write(self: *Self, byte: u8) void {
    self.codes.append(.{ .byte = byte }) catch unreachable;
}

pub fn writeCode(self: *Self, code: OpCode) void {
    self.codes.append(.{ .op_code = code }) catch unreachable;
}

pub fn addConstant(self: *Self, value: Value) u8 {
    // TODO(yemon): the constants 'added position' is going to be stored as 
    // [OP_CONSTANT][index] as 2-bytes instruction.
    // So potentially, the size of the `len` should be 1 byte (u8)
    // NOTE(yemon): custom `ArrayList` type??
    self.constants.append(value) catch unreachable;
    const pos: u8 = @intCast(self.constants.items.len-1);
    return pos;
}

pub fn disassemble(self: *const Self, comptime label: []const u8) void {
    const title = "== " ++ label ++ " ==";
    debug.print(title ++ "\n", .{});
    const sep = "-" ** title.len;
    debug.print(sep ++ "\n", .{});

    var offset: usize = 0;
    while (offset < self.codes.items.len) {
        offset = self.disassembleInstruction(offset);
    }
}

fn disassembleInstruction(self: *const Self, offset: usize) usize {
    debug.print("{:0>4}  ", .{ offset });
    const instruction: Byte = self.codes.items[offset];

    const new_offset: usize = display: switch (instruction) {
        .op_code => |code| {
            const size = code.displayOffset();
            break :display (offset + size);
        },
        .byte => |byte| {
            debug.print("{any}", .{ byte });
            break :display (offset + 1);
        },
    };

    debug.print("\n", .{});
    return new_offset;
}

// TODO(yemon): Come up with a streamlined way to "disassemble" both 
// the op codes and the data bytes together, only with "offset" control
fn constantInstruction(self: *const Self, offset: usize) usize {
    _ = self;
    _ = offset;
}
