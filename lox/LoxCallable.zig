const std = @import("std");
const debug = @import("std").debug;
const Allocator = @import("std").mem.Allocator;

const Value = @import("interpreter.zig").Value;
const Interpreter = @import("interpreter.zig");

pub fn LoxCallable(comptime SourceType: type) type {
    _ = SourceType;
    return struct {
        const Self = @This();

        allocator: Allocator,

        pub fn init(allocator: Allocator) Self {
            return Self{
                .allocator = allocator,
            };
        }

        pub fn arity(self: *const Self) u32 {
            _ = self;
            return 0;
        }

        pub fn call(self: *Self, allocator: Allocator, interpreter: *Interpreter, arguments: *std.ArrayList(Value)) Value {
            _ = self;
            _ = allocator;
            _ = interpreter;
            _ = arguments;
            debug.print("The function is being called now!!\n", .{});
            return Value{ .nil = true };
        }
    };
}
